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
    private LangController langController;

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
        saveDefaultLanguageFiles();
        loadMysqlConfigFromConfig();

        // Nyelvkezelő inicializálása
        langController = new LangController(this);

        logInfo(getMessage("plugin.starting", "Plugin indítása..."));
        logInfo(getMessage("db.checking", "MySQL ellenőrzés..."));

        try {
            mysql = new MySQLManager(cfgHost, cfgPort, cfgDatabase, cfgUser, cfgPassword, cfgTable, this);
            mysql.createTableIfNotExists();
            snapshotMysqlConfig();
            logInfo(getMessage("db.connect_success", "MySQL kapcsolat sikeres. Plugin engedélyezve."));
            logInfo(getMessage("db.table_created", "Tábla létrehozva: %table%").replace("%table%", cfgTable));
        } catch (SQLException e) {
            logError(getMessage("db.connect_failure", "Nem sikerült kapcsolódni a MySQL-hez — plugin letiltva."));
            logError(getMessage("errors.db_check_error", "Hiba: %error%").replace("%error%", e.getMessage()));
            Bukkit.getPluginManager().disablePlugin(this);
            return;
        }
        PluginManager pm = getServer().getPluginManager();
        Listener loginListener = new LoginListener(this, mysql, langController);
        pm.registerEvents(loginListener, this);

        if (getCommand("discordlinker") != null) {
            ReloadCommand reloadCmd = new ReloadCommand(this);
            this.getCommand("discordlinker").setExecutor(reloadCmd);
            this.getCommand("discordlinker").setTabCompleter(reloadCmd);
        } else {
            logWarn(getMessage("plugin.command_not_found", "plugin.yml nem tartalmaz 'discordlinker' parancsot!"));
        }

        logInfo(getMessage("plugin.enabled", "DiscordLinker engedélyezve."));
    }
    @Override
    public void onDisable() {
        if (mysql != null) mysql.close();
        logInfo(getMessage("plugin.disabled", "DiscordLinker leállítva."));
    }

    public String getMessage(String path, String fallback) {
        if (langController == null) return fallback;
        return langController.getMessage(path, fallback);
    }

    public LangController getLangController() {
        return langController;
    }

    /**
     * Mentés az alapértelmezett nyelvfájlokat a 'langs' mappába
     */
    private void saveDefaultLanguageFiles() {
        String[] langFiles = {"hu_messages.yml", "en_messages.yml"};
        for (String fileName : langFiles) {
            saveLanguageResource(fileName);
        }
    }

    /**
     * Egy nyelvfájl mentése
     */
    private void saveLanguageResource(String fileName) {
        File langsDir = new File(getDataFolder(), "langs");
        if (!langsDir.exists()) {
            langsDir.mkdirs();
        }
        File langFile = new File(langsDir, fileName);
        if (!langFile.exists()) {
            saveResource("langs/" + fileName, false);
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
            String msg = "config.yml syntax hibás: " + ice.getMessage();
            logError(msg);
            return new ReloadResult(false, msg, NamedTextColor.RED);
        } catch (IOException ioe) {
            String msg = "config.yml olvasási hiba: " + ioe.getMessage();
            logError(msg);
            return new ReloadResult(false, msg, NamedTextColor.RED);
        }

        reloadConfig();
        loadMysqlConfigFromConfig();
        
        // Nyelvfájlok újratöltése
        langController.reloadLanguages();

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
                String msg = getMessage("reload.mysql_failed", "Reload fail, nincs mysql kapcsolat: %error%").replace("%error%", ex.getMessage());
                logError(msg);
                if (newMysql != null) newMysql.close();
                return new ReloadResult(false, msg, NamedTextColor.RED);
            }

            if (mysql != null) mysql.close();
            mysql = newMysql;
            snapshotMysqlConfig();
            String msg = getMessage("reload.mysql_success", "MySQL kapcsolat frissítve.");
            logInfo(msg);
        } else {
            logInfo(getMessage("reload.db_config_not_changed", "MySQL konfiguráció nem változott."));
        }

        String successMsg = getMessage("reload.success", "Plugin újratöltve.");
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
