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
    private final LangController langController;
    private final SecureRandom random = new SecureRandom();

    public LoginListener(Plugin plugin, MySQLManager mysql, LangController langController) {
        this.plugin = plugin;
        this.mysql = mysql;
        this.langController = langController;
    }

    @EventHandler
    public void onPreLogin(AsyncPlayerPreLoginEvent event) {
        String name = event.getName();
        String uuid = event.getUniqueId().toString();

        try {
            MySQLManager.Record rec = mysql.getRecord(name);

            if (rec == null) {
                int len = plugin.getConfig().getInt("code.length", 8);
                int minutes = plugin.getConfig().getInt("code.expiration_minutes", 5);
                String code = generateCode(len);
                Timestamp now = new Timestamp(System.currentTimeMillis());
                Timestamp exp = new Timestamp(System.currentTimeMillis() + minutes * 60L * 1000L);

                mysql.insertNewCode(name, uuid, code, now, exp);
                String template = langController.getMessage("kick.not_linked", "Fiókod nincs összekötve. Kód: %code%");
                String kickMsg = template.replace("%code%", code).replace("%minutes%", String.valueOf(minutes));
                event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(kickMsg));
                return;
            }

            if (rec.discordUserId != null) {
                if (Boolean.TRUE.equals(rec.accepted)) {
                    return; // engedélyezett
                } else if (Boolean.FALSE.equals(rec.accepted)) {
                    String reason = rec.rejectReason != null ? rec.rejectReason : langController.getMessage("kick.rejected_default", "A jelentkezésed elutasításra került.");
                    String template = langController.getMessage("kick.rejected", "Téged a következő indokkal utasítottak el: %reason%");
                    String msg = template.replace("%reason%", reason);
                    event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(msg));
                    return;
                } else {
                    String msg = langController.getMessage("kick.pending", "A jelentkezésed folyamatban van, kérlek várj.");
                    event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(msg));
                    return;
                }
            }

            // nincs linkelve -> ellenőrizni a lejáratot
            Timestamp now = new Timestamp(System.currentTimeMillis());
            if (rec.expirationAt != null && rec.expirationAt.before(now)) {
                int len = plugin.getConfig().getInt("code.length", 8);
                int minutes = plugin.getConfig().getInt("code.expiration_minutes", 5);
                String code = generateCode(len);
                Timestamp created = now;
                Timestamp exp = new Timestamp(System.currentTimeMillis() + minutes * 60L * 1000L);

                mysql.updateCodeIfNotLinked(name, uuid, code, created, exp);
                String template = langController.getMessage("kick.not_linked", "Fiókod nincs összekötve. Kód: %code%");
                String kickMsg = template.replace("%code%", code).replace("%minutes%", String.valueOf(minutes));
                event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(kickMsg));
                return;
            } else {
                String code = rec.generatedCode;
                int minutes = (int) Math.max(1, (rec.expirationAt.getTime() - now.getTime()) / 60000L);
                String template = langController.getMessage("kick.not_linked", "Fiókod nincs összekötve. Kód: %code%");
                String kickMsg = template.replace("%code%", code).replace("%minutes%", String.valueOf(minutes));
                event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(kickMsg));
                return;
            }

        } catch (SQLException e) {
            plugin.getLogger().severe("DB hiba belépéskor: " + e.getMessage());
            String err = langController.getMessage("errors.db_check_error", "DB hiba: %error%").replace("%error%", e.getMessage());
            event.disallow(AsyncPlayerPreLoginEvent.Result.KICK_OTHER, Component.text(err));
        } catch (Exception ex) {
            plugin.getLogger().severe("Hiba belépés közben: " + ex.getMessage());
            String msg = langController.getMessage("errors.login_error", "Szerver hiba. Próbáld később.");
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
