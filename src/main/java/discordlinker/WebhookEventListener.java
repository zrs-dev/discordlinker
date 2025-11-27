package discordlinker;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.plugin.Plugin;

/**
 * Discord Webhook Event Listener
 * Küld webhook üzeneteket a bejelentkezéskor és kilépéskor (ha engedélyezve van)
 */
public class WebhookEventListener implements Listener {
    private final WebhookManager webhookManager;
    private final PlayerUUIDCache uuidCache;

    public WebhookEventListener(Plugin plugin, WebhookManager webhookManager, PlayerUUIDCache uuidCache, APIClient apiClient) {
        this.webhookManager = webhookManager;
        this.uuidCache = uuidCache;
        // Az APIClient már nem szükséges itt (az API-t csak a LoginListener hívja az új join request-nél)
    }

    /**
     * Amikor játékos belép a szerverhez (már engedélyezve van a belépése)
     */
    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent event) {
        String playerName = event.getPlayer().getName();
        var playerUUID = event.getPlayer().getUniqueId();
        
        // UUID cacheling a kilépéshez
        uuidCache.putPlayer(playerName, playerUUID);
        
        // Webhook küldése, ha engedélyezve van
        webhookManager.sendPlayerJoinedWebhook(playerName, playerUUID);
    }

    /**
     * Amikor játékos kilép a szerverről
     */
    @EventHandler
    public void onPlayerQuit(PlayerQuitEvent event) {
        String playerName = event.getPlayer().getName();
        var playerUUID = event.getPlayer().getUniqueId();
        String reason = event.getReason().name(); // pl. "KICKED", "QUIT", stb.
        
        // Webhook küldése, ha engedélyezve van (UUID-val)
        webhookManager.sendPlayerLeftWebhook(playerName, playerUUID, reason);
        
        // UUID eltávolítása a cachéből (memória tisztítás)
        uuidCache.removePlayer(playerName);
    }
}

