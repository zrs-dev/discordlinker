package discordlinker;

import java.util.concurrent.ConcurrentHashMap;

/**
 * Játékosok korábbi locale-jait tárolja
 * Segít a login előtt meghatározni a nyelvét (AsyncPlayerPreLoginEvent)
 */
public class LocaleCache {
    private static final ConcurrentHashMap<String, String> playerLocales = new ConcurrentHashMap<>();

    /**
     * Mentsd el a játékos locale-ját (PlayerJoinEvent-ből)
     */
    public static void setPlayerLocale(String playerName, String locale) {
        playerLocales.put(playerName.toLowerCase(), locale);
    }

    /**
     * Lekérd a játékos utolsó ismert locale-ját
     * Ha nem ismert, null-t ad vissza
     */
    public static String getPlayerLocale(String playerName) {
        return playerLocales.get(playerName.toLowerCase());
    }

    /**
     * Távolítsd el a játékos locale-ját (logout-kor, ha memóriát szeretnél spórolni)
     */
    public static void removePlayerLocale(String playerName) {
        playerLocales.remove(playerName.toLowerCase());
    }

    /**
     * Cache mérete (debug)
     */
    public static int cacheSize() {
        return playerLocales.size();
    }
}

