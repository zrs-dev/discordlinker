package discordlinker;

import net.kyori.adventure.text.Component;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.AsyncPlayerPreLoginEvent;
import org.bukkit.plugin.Plugin;

import java.security.SecureRandom;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Base64;

public class LoginListener implements Listener {
    private final Plugin plugin;
    private final MySQLManager mysql;
    private final MessageManager messageManager;
    private final PlayerUUIDCache uuidCache;
    private final APIClient apiClient;
    private final SecureRandom random = new SecureRandom();

    public LoginListener(Plugin plugin, MySQLManager mysql, MessageManager messageManager, APIClient apiClient, PlayerUUIDCache uuidCache) {
        this.plugin = plugin;
        this.mysql = mysql;
        this.messageManager = messageManager;
        this.apiClient = apiClient;
        this.uuidCache = uuidCache;
    }

    @EventHandler
    public void onPreLogin(AsyncPlayerPreLoginEvent event) {
        String name = event.getName();
        String uuid;
        // If server is in offline mode, attempt to resolve the official Mojang UUID for the account.
        try {
            boolean onlineMode = plugin.getServer().getOnlineMode();
            if (!onlineMode && uuidCache != null) {
                java.util.UUID resolved = uuidCache.resolveOfficialUUID(name);
                if (resolved != null) {
                    uuid = resolved.toString();
                    String debugResolved = messageManager.getMessage("debug.uuid_resolved", "[DEBUG UUID] %player% resolved to Mojang UUID: %uuid%")
                            .replace("%player%", name).replace("%uuid%", uuid);
                    plugin.getLogger().info(debugResolved);
                } else {
                    uuid = event.getUniqueId().toString();
                }
            } else {
                uuid = event.getUniqueId().toString();
            }
        } catch (Throwable t) {
            uuid = event.getUniqueId().toString();
        }
        
        String debugLoginTry = messageManager.getMessage("debug.login_try", "[DEBUG LOGIN] %player% (UUID: %uuid%) is trying to join.")
            .replace("%player%", name).replace("%uuid%", uuid);
        plugin.getLogger().info(debugLoginTry);

        try {
            MySQLManager.Record rec = mysql.getRecord(name);
            
            if (rec == null) {
                String d = messageManager.getMessage("debug.login_no_record", "[DEBUG LOGIN] %player% — no record, generating new code.").replace("%player%", name);
                plugin.getLogger().info(d);
                int len = plugin.getConfig().getInt("code.length", 8);
                int minutes = plugin.getConfig().getInt("code.expiration_minutes", 5);
                String code = generateCode(len);
                Timestamp now = new Timestamp(System.currentTimeMillis());
                Timestamp exp = new Timestamp(System.currentTimeMillis() + minutes * 60L * 1000L);

                // New join request — call API
                if (apiClient != null && apiClient.isEnabled()) {
                    apiClient.sendPlayerJoin(name, uuid);
                }

                mysql.insertNewCode(name, uuid, code, now, exp);
                String template = messageManager.getMessage("login.not_linked", "Your account is not linked. Code: %code%\nValid for: %minutes% minutes");
                String kickMsg = template.replace("%code%", code).replace("%minutes%", String.valueOf(minutes));
                String kickLog = messageManager.getMessage("debug.kick_not_linked", "[DEBUG KICK] %player% kicked (not_linked): %message%")
                    .replace("%player%", name).replace("%message%", kickMsg);
                plugin.getLogger().info(kickLog);
                event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(kickMsg));
                return;
            }

            if (rec.discordUserId != null) {
                if (Boolean.TRUE.equals(rec.accepted)) {
                    plugin.getLogger().info(messageManager.getMessage("debug.login_allowed", "[DEBUG LOGIN] %player% — allowed to join.").replace("%player%", name));
                    return; // allowed
                } else if (Boolean.FALSE.equals(rec.accepted)) {
                    plugin.getLogger().info(messageManager.getMessage("debug.login_rejected", "[DEBUG LOGIN] %player% — rejected.").replace("%player%", name));
                    String reason = rec.rejectReason != null ? rec.rejectReason : messageManager.getMessage("login.rejected_default", "Your login request was rejected.");
                    String template = messageManager.getMessage("login.rejected", "Your login was rejected for the following reason: %reason%");
                    String msg = template.replace("%reason%", reason);
                    String kickRejectedLog = messageManager.getMessage("debug.kick_rejected", "[DEBUG KICK] %player% kicked (rejected): %message%")
                            .replace("%player%", name).replace("%message%", msg);
                    plugin.getLogger().info(kickRejectedLog);
                    event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(msg));
                    return;
                } else {
                    plugin.getLogger().info(messageManager.getMessage("debug.login_pending", "[DEBUG LOGIN] %player% — pending.").replace("%player%", name));
                    String msg = messageManager.getMessage("login.pending", "Your login is pending; please wait.");
                    String kickPendingLog = messageManager.getMessage("debug.kick_pending", "[DEBUG KICK] %player% kicked (pending): %message%")
                            .replace("%player%", name).replace("%message%", msg);
                    plugin.getLogger().info(kickPendingLog);
                    event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(msg));
                    return;
                }
            }

            // not linked -> check expiration
            Timestamp now = new Timestamp(System.currentTimeMillis());
            if (rec.expirationAt != null && rec.expirationAt.before(now)) {
                plugin.getLogger().info(messageManager.getMessage("debug.login_code_expired", "[DEBUG LOGIN] %player% — code expired, generating new code.").replace("%player%", name));
                int len = plugin.getConfig().getInt("code.length", 8);
                int minutes = plugin.getConfig().getInt("code.expiration_minutes", 5);
                String code = generateCode(len);
                Timestamp created = now;
                Timestamp exp = new Timestamp(System.currentTimeMillis() + minutes * 60L * 1000L);

                mysql.updateCodeIfNotLinked(name, uuid, code, created, exp);
                String template = messageManager.getMessage("login.not_linked", "Your account is not linked. Code: %code%\nValid for: %minutes% minutes");
                String kickMsg = template.replace("%code%", code).replace("%minutes%", String.valueOf(minutes));
                String kickExpiredLog = messageManager.getMessage("debug.kick_not_linked", "[DEBUG KICK] %player% kicked (not_linked): %message%")
                    .replace("%player%", name).replace("%message%", kickMsg);
                plugin.getLogger().info(kickExpiredLog);
                event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(kickMsg));
                return;
            } else {
                plugin.getLogger().info(messageManager.getMessage("debug.login_code_valid_not_linked", "[DEBUG LOGIN] %player% — code valid but not linked yet.").replace("%player%", name));
                String code = rec.generatedCode;
                int minutes = (int) Math.max(1, (rec.expirationAt.getTime() - now.getTime()) / 60000L);
                String template = messageManager.getMessage("login.not_linked", "Your account is not linked. Code: %code%\nValid for: %minutes% minutes");
                String kickMsg = template.replace("%code%", code).replace("%minutes%", String.valueOf(minutes));
                String kickValidLog = messageManager.getMessage("debug.kick_not_linked", "[DEBUG KICK] %player% kicked (not_linked): %message%")
                    .replace("%player%", name).replace("%message%", kickMsg);
                plugin.getLogger().info(kickValidLog);
                event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(kickMsg));
                return;
            }

        } catch (SQLException e) {
            plugin.getLogger().severe(messageManager.getMessage("debug.error_sql", "[DEBUG ERROR] %player% — SQLException: %error%")
                .replace("%player%", name).replace("%error%", e.getMessage()));
            plugin.getLogger().severe(messageManager.getMessage("database.check_error", "DB error: %error%").replace("%error%", e.getMessage()));
            String err = messageManager.getMessage("database.check_error", "DB error: %error%").replace("%error%", e.getMessage());
            String kickDbLog = messageManager.getMessage("debug.kick_db_error", "[DEBUG KICK] %player% kicked (db_error): %message%")
                .replace("%player%", name).replace("%message%", err);
            plugin.getLogger().info(kickDbLog);
            event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(err));
        } catch (Exception ex) {
            plugin.getLogger().severe(messageManager.getMessage("debug.error_general", "[DEBUG ERROR] %player% — General error: %error%")
                .replace("%player%", name).replace("%error%", ex.getMessage()));
            plugin.getLogger().severe("Error during login: " + ex.getMessage());
            String msg = messageManager.getMessage("login.error", "Server error. Please try again later.");
            String kickGeneralLog = messageManager.getMessage("debug.kick_general_error", "[DEBUG KICK] %player% kicked (general_error): %message%")
                .replace("%player%", name).replace("%message%", msg);
            plugin.getLogger().info(kickGeneralLog);
            event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(msg));
        }
    }

    private String generateCode(int len) {
        byte[] b = new byte[len];
        random.nextBytes(b);
        String s = Base64.getUrlEncoder().withoutPadding().encodeToString(b).replaceAll("[^A-Za-z0-9]", "");
        if (s.length() > len) s = s.substring(0, len);
        while (s.length() < len) s += (char) ('A' + random.nextInt(26));
        return s.toUpperCase();
    }
}
