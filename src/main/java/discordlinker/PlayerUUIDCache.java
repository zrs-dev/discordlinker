package discordlinker;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Játékosok UUID-jeit cacheli a csatlakozás/kilépés között
 * A disconnect event-ben tudunk UUID-t lekérni, ha a Player objektum már nem elérhető
 */
public class PlayerUUIDCache {
    private final Map<String, UUID> playerUUIDs;

    public PlayerUUIDCache() {
        this.playerUUIDs = new HashMap<>();
    }

    /**
     * Egy játékos UUID-jét cacheli (csatlakozáskor)
     */
    public void putPlayer(String playerName, UUID uuid) {
        if (playerName != null && uuid != null) {
            playerUUIDs.put(playerName, uuid);
        }
    }

    /**
     * Egy játékos UUID-jét lekéri (kilépéskor)
     */
    public UUID getPlayer(String playerName) {
        return playerUUIDs.getOrDefault(playerName, null);
    }

    /**
     * Egy játékost eltávolít a cachéből (pl. kilépés után)
     */
    public void removePlayer(String playerName) {
        playerUUIDs.remove(playerName);
    }

    /**
     * Cache lekérdezése (debug)
     */
    public Map<String, UUID> getCache() {
        return new HashMap<>(playerUUIDs);
    }

    /**
     * Cache kiürítése
     */
    public void clear() {
        playerUUIDs.clear();
    }
}
