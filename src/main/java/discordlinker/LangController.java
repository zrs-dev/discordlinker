package discordlinker;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.plugin.java.JavaPlugin;

import java.io.File;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class LangController {
    private final JavaPlugin plugin;
    private final Map<String, FileConfiguration> langCache = new ConcurrentHashMap<>();
    private String defaultLang;
    private final Map<Player, String> playerLanguages = new ConcurrentHashMap<>();

    public LangController(JavaPlugin plugin) {
        this.plugin = plugin;
        this.defaultLang = plugin.getConfig().getString("default_lang", "hu");
        loadAllLanguages();
    }

    /**
     * Betölti az összes elérhető nyelvfájlt a "langs" mappából
     */
    private void loadAllLanguages() {
        File langsDir = new File(plugin.getDataFolder(), "langs");
        if (!langsDir.exists()) {
            langsDir.mkdirs();
            plugin.getLogger().info("[LangController] 'langs' folder created.");
        }

        File[] files = langsDir.listFiles((dir, name) -> name.endsWith("_messages.yml"));
        if (files == null || files.length == 0) {
            plugin.getLogger().warning("[LangController] No language files found in 'langs' folder!");
            return;
        }

        for (File file : files) {
            String langCode = extractLangCode(file.getName());
            try {
                FileConfiguration config = YamlConfiguration.loadConfiguration(file);
                langCache.put(langCode, config);
                plugin.getLogger().info("[LangController] Language file loaded: " + langCode);
            } catch (Exception e) {
                plugin.getLogger().severe("[LangController] Error loading " + file.getName() + ": " + e.getMessage());
            }
        }

        // Ellenőrizd, hogy az alapértelmezett nyelv betöltődött-e
        if (!langCache.containsKey(defaultLang)) {
            plugin.getLogger().warning("[LangController] The default language '" + defaultLang + "' is not available!");
            if (!langCache.isEmpty()) {
                defaultLang = langCache.keySet().iterator().next();
                plugin.getLogger().info("[LangController] Default language changed to: " + defaultLang);
            }
        }
    }

    /**
     * Kivonja a nyelvkódot a fájlnévből
     * pl. "hu_messages.yml" -> "hu", "en-US_messages.yml" -> "en-US"
     */
    private String extractLangCode(String fileName) {
        return fileName.replace("_messages.yml", "");
    }

    /**
     * Meghatározza a játékos nyelvét az MC beállítások alapján
     */
    public String getPlayerLanguage(Player player) {
        // Ha már cachelve van, használd azt
        if (playerLanguages.containsKey(player)) {
            return playerLanguages.get(player);
        }

        // Próbáld meg az MC nyelvbeállítást lekérni
        @SuppressWarnings("deprecation")
        String mcLocale = player.getLocale();

        // Konvertáld MC formátumot standard formátumra: en_us -> en, hu_hu -> hu
        String langCode = convertMCLocaleToLangCode(mcLocale);

        // Ha a nyelv nem elérhető, használd az alapértelmezettet
        if (!langCache.containsKey(langCode)) {
            langCode = defaultLang;
        }

        playerLanguages.put(player, langCode);
        return langCode;
    }

    /**
     * Konvertálja a Minecraft locale formátumot standard formátumra
     * pl. "en_us" -> "en", "hu_hu" -> "hu", "en_us" -> "en-US" (ha létezik)
     */
    private String convertMCLocaleToLangCode(String mcLocale) {
        if (mcLocale == null || mcLocale.isEmpty()) {
            return defaultLang;
        }

        // Próbáld meg a teljes locale-t (pl. en-US)
        String fullLocale = mcLocale.replace("_", "-");
        if (langCache.containsKey(fullLocale)) {
            return fullLocale;
        }

        // Ha nincs, próbáld csak az első részt (pl. en)
        String langOnly = fullLocale.split("-")[0].toLowerCase();
        if (langCache.containsKey(langOnly)) {
            return langOnly;
        }

        return defaultLang;
    }

    /**
     * Üzenet lekérése a játékos nyelvén
     */
    public String getMessage(Player player, String key, String fallback) {
        String langCode = getPlayerLanguage(player);
        return getMessageByLang(langCode, key, fallback);
    }

    /**
     * Üzenet lekérése egy adott nyelvkóddal
     */
    public String getMessageByLang(String langCode, String key, String fallback) {
        FileConfiguration config = langCache.get(langCode);
        if (config == null) {
            config = langCache.get(defaultLang);
        }
        if (config == null) {
            return fallback;
        }
        return config.getString(key, fallback);
    }

    /**
     * Üzenet lekérése a konzolhoz (alapértelmezett nyelv)
     */
    public String getMessage(String key, String fallback) {
        return getMessageByLang(defaultLang, key, fallback);
    }

    /**
     * Új nyelvfájl dinamikus betöltése (reload után)
     */
    public void reloadLanguages() {
        langCache.clear();
        playerLanguages.clear();
        this.defaultLang = plugin.getConfig().getString("default_lang", "hu");
        loadAllLanguages();
        plugin.getLogger().info("[LangController] Language files reloaded.");
    }

    /**
     * Játékos nyelvének ürítése a cache-ből (pl. kilépéskor)
     */
    public void removePlayerLanguage(Player player) {
        playerLanguages.remove(player);
    }

    /**
     * Elérhető nyelvek lekérése
     */
    public Set<String> getAvailableLanguages() {
        return new HashSet<>(langCache.keySet());
    }

    /**
     * Hiba üzenet lekérése (konzol)
     */
    public String getErrorMessage(String key, String fallback) {
        return getMessage(key, fallback);
    }

    /**
     * Nyelvkód lekérése egy localeból (pl. "en_us" -> "en", "hu_hu" -> "hu")
     * Szükséges login előtt, amikor még nincs Player objektum
     */
    public String getLangCodeFromLocale(String mcLocale) {
        String langCode = convertMCLocaleToLangCode(mcLocale);
        if (!langCache.containsKey(langCode)) {
            langCode = defaultLang;
        }
        return langCode;
    }

    /**
     * Az alapértelmezett nyelvkód lekérése
     */
    public String getDefaultLangCode() {
        return defaultLang;
    }
}
