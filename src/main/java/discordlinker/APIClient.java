package discordlinker;

import com.google.gson.JsonObject;
import org.bukkit.plugin.Plugin;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

/**
 * API kliens — titkosított adatok küldése saját szerverre
 */
public class APIClient {
    private final Plugin plugin;
    private final String apiUrl;
    private final APIEncryption encryption;

    public APIClient(Plugin plugin, String apiUrl, String apiKey, String encryptionAlgorithm) {
        this.plugin = plugin;
        this.apiUrl = apiUrl;
        this.encryption = new APIEncryption(apiKey, encryptionAlgorithm);
    }

    /**
     * Játékos csatlakozási adatainak küldése az API szerverre
     * @param playerName A játékos felhasználóneve
     * @param playerUUID A játékos UUID-je
     */
    public void sendPlayerJoin(String playerName, String playerUUID) {
        if (apiUrl == null || apiUrl.isEmpty()) {
            return;
        }

        CompletableFuture.runAsync(() -> {
            try {
                // Payload készítése: csak felhasználónév és UUID
                JsonObject data = new JsonObject();
                data.addProperty("username", playerName);
                data.addProperty("uuid", playerUUID);

                // Titkosítás
                String encrypted = encryption.encrypt(data.toString());

                // Nyers titkosított adat küldése
                sendPost(encrypted, "join");
            } catch (Exception e) {
                plugin.getLogger().severe("[APIClient] Hiba a join adatok küldésekor: " + e.getMessage());
                e.printStackTrace();
            }
        });
    }

    /**
     * Játékos kilépési adatainak küldése az API szerverre
     * @param playerName A játékos felhasználóneve
     * @param playerUUID A játékos UUID-je
     * @param reason Kilépési ok
     */
    public void sendPlayerQuit(String playerName, String playerUUID, String reason) {
        if (apiUrl == null || apiUrl.isEmpty()) {
            return;
        }

        CompletableFuture.runAsync(() -> {
            try {
                // Payload készítése: csak felhasználónév és UUID
                JsonObject data = new JsonObject();
                data.addProperty("username", playerName);
                data.addProperty("uuid", playerUUID);

                // Titkosítás
                String encrypted = encryption.encrypt(data.toString());

                // Nyers titkosított adat küldése
                sendPost(encrypted, "quit");
            } catch (Exception e) {
                plugin.getLogger().severe("[APIClient] Hiba a quit adatok küldésekor: " + e.getMessage());
                e.printStackTrace();
            }
        });
    }

    /**
     * HTTP POST kérés küldése az API szerverre
     * Nyers titkosított adat küldése (semmilyen burkolat vagy metaadat nélkül)
     */
    private void sendPost(String rawEncryptedData, String eventType) throws Exception {
        HttpURLConnection connection = (HttpURLConnection) URI.create(apiUrl).toURL().openConnection();
        connection.setRequestMethod("POST");
        connection.setRequestProperty("Content-Type", "text/plain");
        connection.setRequestProperty("User-Agent", "DiscordLinker/1.0");
        connection.setDoOutput(true);
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);

        // Nyers titkosított adat küldése
        byte[] input = rawEncryptedData.getBytes(StandardCharsets.UTF_8);
        try (OutputStream os = connection.getOutputStream()) {
            os.write(input, 0, input.length);
        }

        // Válasz ellenőrzése
        int responseCode = connection.getResponseCode();
        if (responseCode >= 200 && responseCode < 300) {
            plugin.getLogger().info("[APIClient] API " + eventType + " request sikeres: HTTP " + responseCode);
        } else {
            plugin.getLogger().warning("[APIClient] API " + eventType + " request hiba: HTTP " + responseCode);
        }

        connection.disconnect();
    }

    /**
     * API kliens engedélyezve-e?
     */
    public boolean isEnabled() {
        return apiUrl != null && !apiUrl.isEmpty();
    }
}
