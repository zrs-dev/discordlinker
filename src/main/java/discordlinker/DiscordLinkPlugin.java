package discordlinker;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.Listener;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.java.JavaPlugin;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;


public class DiscordLinkPlugin extends JavaPlugin {
    private MySQLManager mysql;
    private MessageManager messageManager;
    private WebhookManager webhookManager;
    private PlayerUUIDCache uuidCache;
    private APIClient apiClient;

    private String cfgHost;
    private int cfgPort;
    private String cfgDatabase;
    private String cfgUser;
    private String cfgPassword;
    private String cfgTable;

    private String lastHost;
    private int lastPort;
    private String lastDatabase;
    private String lastUser;
    private String lastPassword;
    private String lastTable;

    @Override
    public void onEnable() {
        saveDefaultConfig();
        saveDefaultMessagesFile();
        saveDefaultWebhookTemplates();
        loadMysqlConfigFromConfig();

        // Üzenet kezelő inicializálása
        messageManager = new MessageManager(this);
        
        // UUID Cache inicializálása
        uuidCache = new PlayerUUIDCache();

        logInfo(getMessage("plugin.starting", "Starting plugin..."));
        logInfo(getMessage("database.checking", "Checking MySQL connection..."));

        try {
            mysql = new MySQLManager(cfgHost, cfgPort, cfgDatabase, cfgUser, cfgPassword, cfgTable, this);
            mysql.createTableIfNotExists();
            snapshotMysqlConfig();
            logInfo(getMessage("database.connect_success", "MySQL connection successful. Plugin enabled."));
            logInfo(getMessage("database.table_created", "Table created: %table%").replace("%table%", cfgTable));
        } catch (SQLException e) {
            logError(getMessage("database.connect_failure", "Failed to connect to MySQL — plugin disabled."));
            logError(getMessage("database.check_error", "DB error: %error%").replace("%error%", e.getMessage()));
            Bukkit.getPluginManager().disablePlugin(this);
            return;
        }
        PluginManager pm = getServer().getPluginManager();
        
        // API kliens inicializálása (opcionális)
        boolean apiEnabled = getConfig().getBoolean("discord.api.enabled", false);
        String apiUrl = getConfig().getString("discord.api.url", "");
        String apiKey = getConfig().getString("discord.api.api_key", "");
        String encryptionAlgo = getConfig().getString("discord.api.encryption_algorithm", "aes-256-cbc");
        
        if (apiEnabled && !apiUrl.isEmpty() && !apiKey.isEmpty()) {
            apiClient = new APIClient(this, apiUrl, apiKey, encryptionAlgo);
            logInfo("[API] API client initialized: " + apiUrl + " (" + encryptionAlgo + ")");
        } else {
            apiClient = null;
            if (apiEnabled) {
                logWarn("[API] API enabled but URL or API Key is missing!");
            }
        }
        
        // LoginListener inicializálása APIClient-tel
        Listener loginListener = new LoginListener(this, mysql, messageManager, apiClient, uuidCache);
        pm.registerEvents(loginListener, this);
        
        // Discord Webhook inicializálása (join és left külön)
        String joinWebhookUrl = getConfig().getString("discord.join.webhook_url", "");
        String joinEmbedType = getConfig().getString("discord.join.embed_type", "embed");
        String leftWebhookUrl = getConfig().getString("discord.left.webhook_url", "");
        String leftEmbedType = getConfig().getString("discord.left.embed_type", "embed");
        
        webhookManager = new WebhookManager(this, joinWebhookUrl, joinEmbedType, leftWebhookUrl, leftEmbedType);
        
        if (webhookManager.isJoinEnabled() || webhookManager.isLeftEnabled()) {
            logInfo(getMessage("webhook.enabled", "Discord Webhook enabled."));
            Listener webhookListener = new WebhookEventListener(this, webhookManager, uuidCache, apiClient);
            pm.registerEvents(webhookListener, this);
        } else {
            logWarn(getMessage("webhook.disabled", "Discord Webhook disabled or no URL configured."));
        }

        if (getCommand("discordlinker") != null) {
            ReloadCommand reloadCmd = new ReloadCommand(this);
            this.getCommand("discordlinker").setExecutor(reloadCmd);
            this.getCommand("discordlinker").setTabCompleter(reloadCmd);
        } else {
            logWarn(getMessage("plugin.command_not_found", "plugin.yml does not contain the 'discordlinker' command!"));
        }

        logInfo(getMessage("plugin.enabled", "DiscordLinker enabled."));
    }
    @Override
    public void onDisable() {
        if (mysql != null) mysql.close();
        logInfo(getMessage("plugin.disabled", "DiscordLinker disabled."));
    }

    public String getMessage(String path, String fallback) {
        if (messageManager == null) return fallback;
        return messageManager.getMessage(path, fallback);
    }

    public MessageManager getMessageManager() {
        return messageManager;
    }

    public WebhookManager getWebhookManager() {
        return webhookManager;
    }

    /**
     * messages.yml mentése
     */
    private void saveDefaultMessagesFile() {
        File messagesFile = new File(getDataFolder(), "messages.yml");
        if (!messagesFile.exists()) {
            saveResource("messages.yml", false);
        }
    }

    /**
     * Webhook sablonok mentése
     */
    private void saveDefaultWebhookTemplates() {
        String[] webhookFiles = {"joined.json", "disconnected.json"};
        for (String fileName : webhookFiles) {
            saveWebhookResource(fileName);
        }
    }

    /**
     * Egy webhook sablon mentése
     */
    private void saveWebhookResource(String fileName) {
        File webhooksDir = new File(getDataFolder(), "webhooks");
        if (!webhooksDir.exists()) {
            webhooksDir.mkdirs();
        }
        File webhookFile = new File(webhooksDir, fileName);
        if (!webhookFile.exists()) {
            saveResource("webhooks/" + fileName, false);
        }
    }


    private void loadMysqlConfigFromConfig() {
        FileConfiguration cfg = getConfig();
        cfgHost = cfg.getString("mysql.host", "127.0.0.1");
        cfgPort = cfg.getInt("mysql.port", 3306);
        cfgDatabase = cfg.getString("mysql.database", "minecraft");
        cfgUser = cfg.getString("mysql.user", "mc_user");
        cfgPassword = cfg.getString("mysql.password", "");
        cfgTable = cfg.getString("mysql.table", "discord_link_requests");
    }

    private void snapshotMysqlConfig() {
        lastHost = cfgHost;
        lastPort = cfgPort;
        lastDatabase = cfgDatabase;
        lastUser = cfgUser;
        lastPassword = cfgPassword;
        lastTable = cfgTable;
    }

    public ReloadResult reloadAll() {
        File configFile = new File(getDataFolder(), "config.yml");
        YamlConfiguration tmpCfg = new YamlConfiguration();
        try {
            tmpCfg.load(configFile);
        } catch (InvalidConfigurationException ice) {
            String msg = "config.yml syntax error: " + ice.getMessage();
            logError(msg);
            return new ReloadResult(false, msg, NamedTextColor.RED);
        } catch (IOException ioe) {
            String msg = "config.yml read error: " + ioe.getMessage();
            logError(msg);
            return new ReloadResult(false, msg, NamedTextColor.RED);
        }

        reloadConfig();
        loadMysqlConfigFromConfig();
        
        // Üzenetek és webhook sablonok újratöltése
        if (messageManager != null) {
            messageManager.reloadMessages();
        }
        if (webhookManager != null) {
            webhookManager.reloadTemplates();
        }

        boolean changed = !equalsNullable(lastHost, cfgHost)
                || lastPort != cfgPort
                || !equalsNullable(lastDatabase, cfgDatabase)
                || !equalsNullable(lastUser, cfgUser)
                || !equalsNullable(lastPassword, cfgPassword)
                || !equalsNullable(lastTable, cfgTable);

        if (changed) {
            logInfo(getMessage("reload.db_config_changed", "MySQL konfiguráció változott — újracsatlakozás kísérlete..."));
            MySQLManager newMysql = null;
            try {
                newMysql = new MySQLManager(cfgHost, cfgPort, cfgDatabase, cfgUser, cfgPassword, cfgTable, this);
                newMysql.createTableIfNotExists();
            } catch (SQLException ex) {
                String msg = getMessage("reload.mysql_failed", "Reload failed, no MySQL connection: %error%").replace("%error%", ex.getMessage());
                logError(msg);
                if (newMysql != null) newMysql.close();
                return new ReloadResult(false, msg, NamedTextColor.RED);
            }

            if (mysql != null) mysql.close();
            mysql = newMysql;
            snapshotMysqlConfig();
            String msg = getMessage("reload.mysql_success", "MySQL connection refreshed.");
            logInfo(msg);
        } else {
            logInfo(getMessage("reload.db_config_not_changed", "MySQL configuration did not change."));
        }
        String successMsg = getMessage("reload.success", "Plugin reloaded.");
        logInfo(successMsg);
        return new ReloadResult(true, successMsg, NamedTextColor.GREEN);
    }

    private static boolean equalsNullable(String a, String b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.equals(b);
    }

    /* ---------------- logging / messaging helpers (Adventure) ---------------- */

    public void logInfo(String message) {
        // Console: use plain logger (no legacy color codes). Players get colored messages via sendMessage.
        getLogger().info("[DiscordLinker] " + message);
    }

    public void logWarn(String message) {
        getLogger().warning("[DiscordLinker] " + message);
    }

    public void logError(String message) {
        getLogger().severe("[DiscordLinker] " + message);
    }

    /**
     * Send a message to a CommandSender. If the target is the console, write to logger (no '§' codes).
     * If it's a player, send an Adventure Component with colors.
     */
    public void sendMessage(CommandSender to, NamedTextColor color, String message) {
        if (to == null) return;

        if (to instanceof org.bukkit.command.ConsoleCommandSender) {
            // Console -> just use appropriate logger level to avoid '§' visible sequences.
            if (color == NamedTextColor.RED) logError(message);
            else if (color == NamedTextColor.YELLOW) logWarn(message);
            else logInfo(message);
            return;
        }

        // Player or other -> send colored Component
        Component pref = Component.text("[DiscordLinker] ").color(NamedTextColor.GREEN);
        if (color == NamedTextColor.RED) pref = Component.text("[DiscordLinker] ").color(NamedTextColor.RED);
        else if (color == NamedTextColor.YELLOW) pref = Component.text("[DiscordLinker] ").color(NamedTextColor.YELLOW);

        Component msg = Component.text(message).color(color);
        try {
            to.sendMessage(pref.append(msg));
        } catch (Throwable t) {
            // fallback to plain text
            to.sendMessage("[DiscordLinker] " + message);
        }
    }

    /* getters */
    public MySQLManager getMysql() { return mysql; }

    /* Small result holder for reloadAll() */
    public static class ReloadResult {
        public final boolean success;
        public final String message;
        public final NamedTextColor color;

        public ReloadResult(boolean success, String message, NamedTextColor color) {
            this.success = success;
            this.message = message;
            this.color = color;
        }
    }
}
