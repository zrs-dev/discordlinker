package discordlinker;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.java.JavaPlugin;

import java.io.File;

/**
 * Egyszerű üzenet kezelés a messages.yml-ből
 */
public class MessageManager {
    private final JavaPlugin plugin;
    private FileConfiguration messages;

    public MessageManager(JavaPlugin plugin) {
        this.plugin = plugin;
        loadMessages();
    }

    /**
     * Betölti a messages.yml-t
     */
    private void loadMessages() {
        File messagesFile = new File(plugin.getDataFolder(), "messages.yml");
        if (!messagesFile.exists()) {
            plugin.getLogger().warning("[MessageManager] messages.yml does not exist!");
            messages = new YamlConfiguration();
        } else {
            try {
                messages = YamlConfiguration.loadConfiguration(messagesFile);
                plugin.getLogger().info("[MessageManager] messages.yml loaded.");
            } catch (Exception e) {
                plugin.getLogger().severe("[MessageManager] Error loading messages.yml: " + e.getMessage());
                messages = new YamlConfiguration();
            }
        }
    }

    /**
     * Üzenet lekérése
     */
    public String getMessage(String key, String fallback) {
        if (messages == null) return fallback;
        return messages.getString(key, fallback);
    }

    /**
     * Üzenetek újratöltése (reload parancs után)
     */
    public void reloadMessages() {
        loadMessages();
        plugin.getLogger().info("[MessageManager] Messages reloaded.");
    }
}

