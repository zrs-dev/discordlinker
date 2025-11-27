package discordlinker;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URI;

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

    /**
     * Try to resolve the official (Mojang) UUID for a player name using Mojang API.
     * If resolution succeeds, the result will be cached and returned. On failure, returns null.
     */
    public UUID resolveOfficialUUID(String playerName) {
        if (playerName == null) return null;
        // Return cached value if present
        UUID cached = getPlayer(playerName);
        if (cached != null) return cached;

        HttpURLConnection conn = null;
        BufferedReader reader = null;
        try {
            URI uri = URI.create("https://api.mojang.com/users/profiles/minecraft/" + playerName);
            URL url = uri.toURL();
            conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(3000);
            conn.setReadTimeout(3000);
            conn.setDoInput(true);

            int code = conn.getResponseCode();
            if (code != 200) return null;

            reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) sb.append(line);
            String body = sb.toString();
            // simple parse for "id":"<hex>"
            int idx = body.indexOf("\"id\"");
            if (idx == -1) return null;
            int colon = body.indexOf(':', idx);
            if (colon == -1) return null;
            int firstQuote = body.indexOf('"', colon);
            if (firstQuote == -1) return null;
            int secondQuote = body.indexOf('"', firstQuote + 1);
            if (secondQuote == -1) return null;
            String rawId = body.substring(firstQuote + 1, secondQuote).trim();
            if (rawId.length() != 32) return null;
            String withHyphens = rawId.substring(0,8) + "-" + rawId.substring(8,12) + "-" + rawId.substring(12,16) + "-" + rawId.substring(16,20) + "-" + rawId.substring(20);
            UUID uuid = UUID.fromString(withHyphens);
            // cache and return
            putPlayer(playerName, uuid);
            return uuid;
        } catch (Exception ignored) {
            return null;
        } finally {
            try { if (reader != null) reader.close(); } catch (Exception ignored) {}
            if (conn != null) conn.disconnect();
        }
    }
}
