package discordlinker;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerLocaleChangeEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.plugin.Plugin;

/**
 * DEBUG: Naplózza a kliens nyelvét (locale)
 * Segít a honosítás debuggolásában
 */
public class LocaleDebugListener implements Listener {
    private final Plugin plugin;
    private final LangController langController;

    public LocaleDebugListener(Plugin plugin, LangController langController) {
        this.plugin = plugin;
        this.langController = langController;
    }

    /**
     * Amikor játékos csatlakozik — naplózd az inicializált locale-ját
     */
    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent event) {
        @SuppressWarnings("deprecation")
        String clientLocale = event.getPlayer().getLocale();
        String langCode = langController.getPlayerLanguage(event.getPlayer());
        
        // Mentsd el a locale-ját a cache-be (login előtt)
        LocaleCache.setPlayerLocale(event.getPlayer().getName(), clientLocale);
        
        plugin.getLogger().info("[DEBUG LOCALE] " + event.getPlayer().getName() +
            " joined. Client locale: '" + clientLocale +
            "' => Selected lang code: '" + langCode + "'");
    }

    /**
     * Amikor játékos nyelvét megváltoztatja a beállításoknál
     */
    @EventHandler
    public void onLocaleChange(PlayerLocaleChangeEvent event) {
        @SuppressWarnings("deprecation")
        String newLocale = event.getLocale();
        String playerName = event.getPlayer().getName();
        
        plugin.getLogger().info("[DEBUG LOCALE CHANGE] " + playerName +
            " changed locale to: '" + newLocale + "'");
        
        // Cache frissítése az új nyelvhez
        LocaleCache.setPlayerLocale(playerName, newLocale);
        langController.removePlayerLanguage(event.getPlayer());
        String newLangCode = langController.getPlayerLanguage(event.getPlayer());
        
        plugin.getLogger().info("[DEBUG LOCALE CHANGE] New lang code: '" + newLangCode +
            "' — Available languages: " + langController.getAvailableLanguages());
    }

    /**
     * Amikor játékos kilép — távolítsd el a cache-ből
     */
    @EventHandler
    public void onPlayerQuit(PlayerQuitEvent event) {
        LocaleCache.removePlayerLocale(event.getPlayer().getName());
        langController.removePlayerLanguage(event.getPlayer());
    }
}

