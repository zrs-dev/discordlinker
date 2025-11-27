package discordlinker;

import com.google.gson.*;
import org.bukkit.plugin.Plugin;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

/**
 * Discord Webhook management
 * Supports separate join and left webhooks
 * JSON-based embed and message sending
 */
public class WebhookManager {
    private final Plugin plugin;
    
    // Join webhook
    private String joinWebhookUrl;
    private String joinEmbedType;
    private JsonObject joinTemplate;
    
    // Left/Disconnect webhook
    private String leftWebhookUrl;
    private String leftEmbedType;
    private JsonObject leftTemplate;
    
    private static final DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    /**
     * WebhookManager inicializálása join és left webhookal
     */
    public WebhookManager(Plugin plugin, String joinUrl, String joinType, String leftUrl, String leftType) {
        this.plugin = plugin;
        this.joinWebhookUrl = joinUrl != null ? joinUrl : "";
        this.joinEmbedType = joinType != null ? joinType : "embed";
        this.leftWebhookUrl = leftUrl != null ? leftUrl : "";
        this.leftEmbedType = leftType != null ? leftType : "embed";
        
        validateEmbedType(joinEmbedType, "join");
        validateEmbedType(leftEmbedType, "left");
        
        loadTemplates();
    }

    /**
     * Validate embed type
     */
    private void validateEmbedType(String type, String webhookType) {
        if (!type.equals("embed") && !type.equals("components")) {
            plugin.getLogger().warning("[WebhookManager] Unknown embed type for " + webhookType + " webhook: " + type + ", falling back to 'embed'.");
            if ("join".equals(webhookType)) {
                this.joinEmbedType = "embed";
            } else {
                this.leftEmbedType = "embed";
            }
        }
    }

    /**
     * Load webhook templates from JSON files
     */
    private void loadTemplates() {
        File dataFolder = plugin.getDataFolder();
        File webhooksDir = new File(dataFolder, "webhooks");
        if (!webhooksDir.exists()) {
            webhooksDir.mkdirs();
        }

        // Join sablon betöltése
        loadJoinTemplate(webhooksDir);
        
        // Left sablon betöltése
        loadLeftTemplate(webhooksDir);
    }

    /**
     * Load or generate join template
     */
    private void loadJoinTemplate(File webhooksDir) {
        String joinName = joinEmbedType.equals("components") ? "joined-components.json" : "joined.json";
        File joinFile = new File(webhooksDir, joinName);

        if (joinFile.exists()) {
            try {
                JsonElement elem = JsonParser.parseString(readFile(joinFile));
                if (elem.isJsonObject()) {
                    joinTemplate = elem.getAsJsonObject();
                    plugin.getLogger().info("[WebhookManager] " + joinName + " loaded (join webhook).");
                } else {
                    plugin.getLogger().warning("[WebhookManager] " + joinName + " is not a valid JSON object!");
                    joinTemplate = new JsonObject();
                }
            } catch (Exception e) {
                plugin.getLogger().warning("[WebhookManager] Error loading " + joinName + ": " + e.getMessage());
                joinTemplate = new JsonObject();
            }
        } else {
            plugin.getLogger().info("[WebhookManager] " + joinName + " does not exist, generating default...");
            generateDefaultJoinTemplate(joinFile, joinEmbedType);
        }
    }

    /**
     * Left sablon betöltése vagy generálása
     */
    private void loadLeftTemplate(File webhooksDir) {
        String leftName = leftEmbedType.equals("components") ? "disconnected-components.json" : "disconnected.json";
        File leftFile = new File(webhooksDir, leftName);

        if (leftFile.exists()) {
            try {
                JsonElement elem = JsonParser.parseString(readFile(leftFile));
                if (elem.isJsonObject()) {
                    leftTemplate = elem.getAsJsonObject();
                    plugin.getLogger().info("[WebhookManager] " + leftName + " loaded (left webhook).");
                } else {
                    plugin.getLogger().warning("[WebhookManager] " + leftName + " is not a valid JSON object!");
                    leftTemplate = new JsonObject();
                }
            } catch (Exception e) {
                plugin.getLogger().warning("[WebhookManager] Error loading " + leftName + ": " + e.getMessage());
                leftTemplate = new JsonObject();
            }
        } else {
            plugin.getLogger().info("[WebhookManager] " + leftName + " does not exist, generating default...");
            generateDefaultLeftTemplate(leftFile, leftEmbedType);
        }
    }

    /**
     * Alapértelmezett join sablon generálása
     */
    private void generateDefaultJoinTemplate(File file, String embedType) {
        try {
            JsonObject template = new JsonObject();
            template.addProperty("content", "");

            // Create embed
            JsonObject embed = new JsonObject();
            embed.addProperty("title", "Player Joined");
            embed.addProperty("description", "%player% has joined the server!");
            embed.addProperty("color", 65280); // Green

            JsonArray fields = new JsonArray();
            fields.add(createField("Player", "%player%", true));
            fields.add(createField("UUID", "%uuid%", true));
            fields.add(createField("Time", "%time%", false));
            embed.add("fields", fields);

            JsonObject footer = new JsonObject();
            footer.addProperty("text", "DiscordLinker Plugin");
            embed.add("footer", footer);

            JsonArray embeds = new JsonArray();
            embeds.add(embed);
            template.add("embeds", embeds);

            // Components generálása (ha szükséges)
            if (embedType.equals("components")) {
                JsonArray components = new JsonArray();
                JsonObject actionRow = new JsonObject();
                actionRow.addProperty("type", 1);

                JsonArray buttons = new JsonArray();
                JsonObject button = new JsonObject();
                button.addProperty("type", 2);
                button.addProperty("style", 5);
                button.addProperty("label", "Profil");
                button.addProperty("url", "https://example.com");
                buttons.add(button);

                actionRow.add("components", buttons);
                components.add(actionRow);
                template.add("components", components);
            }

                writeFile(file, template.toString());
                plugin.getLogger().info("[WebhookManager] " + file.getName() + " successfully generated.");
            joinTemplate = template;
        } catch (Exception e) {
            plugin.getLogger().severe("[WebhookManager] Error generating default join template: " + e.getMessage());
            joinTemplate = new JsonObject();
        }
    }

    /**
     * Alapértelmezett left sablon generálása
     */
    private void generateDefaultLeftTemplate(File file, String embedType) {
        try {
            JsonObject template = new JsonObject();
            template.addProperty("content", "");

            // Create embed
            JsonObject embed = new JsonObject();
            embed.addProperty("title", "Player Left");
            embed.addProperty("description", "%player% has left the server (Reason: %reason%).");
            embed.addProperty("color", 16711680); // Red

            JsonArray fields = new JsonArray();
            fields.add(createField("Player", "%player%", true));
            fields.add(createField("Reason", "%reason%", true));
            fields.add(createField("Time", "%time%", false));
            embed.add("fields", fields);

            JsonObject footer = new JsonObject();
            footer.addProperty("text", "DiscordLinker Plugin");
            embed.add("footer", footer);

            JsonArray embeds = new JsonArray();
            embeds.add(embed);
            template.add("embeds", embeds);

            // Components generálása (ha szükséges)
            if (embedType.equals("components")) {
                JsonArray components = new JsonArray();
                JsonObject actionRow = new JsonObject();
                actionRow.addProperty("type", 1);

                JsonArray buttons = new JsonArray();
                JsonObject button = new JsonObject();
                button.addProperty("type", 2);
                button.addProperty("style", 5);
                button.addProperty("label", "Szerver");
                button.addProperty("url", "https://example.com");
                buttons.add(button);

                actionRow.add("components", buttons);
                components.add(actionRow);
                template.add("components", components);
            }

            writeFile(file, template.toString());
            plugin.getLogger().info("[WebhookManager] " + file.getName() + " successfully generated.");
            leftTemplate = template;
        } catch (Exception e) {
            plugin.getLogger().severe("[WebhookManager] Error generating default left template: " + e.getMessage());
            leftTemplate = new JsonObject();
        }
    }

    /**
     * Field objektum létrehozása embed-hez
     */
    private JsonObject createField(String name, String value, boolean inline) {
        JsonObject field = new JsonObject();
        field.addProperty("name", name);
        field.addProperty("value", value);
        field.addProperty("inline", inline);
        return field;
    }

    /**
     * Fájl tartalmának beolvasása
     */
    private String readFile(File file) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
        }
        return content.toString();
    }

    /**
     * Fájl írása
     */
    private void writeFile(File file, String content) throws IOException {
        file.getParentFile().mkdirs();
        try (FileWriter writer = new FileWriter(file)) {
            // Prettify JSON for better readability
            JsonElement elem = JsonParser.parseString(content);
            Gson gson = new GsonBuilder().setPrettyPrinting().create();
            writer.write(gson.toJson(elem));
        }
    }

    /**
     * Webhook üzenet küldése csatlakozáskor
     */
    public void sendPlayerJoinedWebhook(String playerName, UUID playerUUID) {
        if (!isJoinEnabled()) return;

        CompletableFuture.runAsync(() -> {
            try {
                if (joinTemplate == null || joinTemplate.isJsonNull()) {
                    plugin.getLogger().warning("[WebhookManager] Join template is empty or null!");
                    return;
                }

                JsonObject payload = deepClone(joinTemplate);
                replaceVariablesForJoin(payload, playerName, playerUUID);
                sendWebhook(payload, joinWebhookUrl, "join");
                plugin.getLogger().info("[WebhookManager] Join webhook sent: " + playerName);
            } catch (Exception e) {
                plugin.getLogger().severe("[WebhookManager] Error sending join webhook: " + e.getMessage());
                e.printStackTrace();
            }
        });
    }

    /**
     * Webhook üzenet küldése kilépéskor
     */
    public void sendPlayerLeftWebhook(String playerName, UUID playerUUID, String reason) {
        if (!isLeftEnabled()) return;

        CompletableFuture.runAsync(() -> {
            try {
                if (leftTemplate == null || leftTemplate.isJsonNull()) {
                    plugin.getLogger().warning("[WebhookManager] Left template is empty or null!");
                    return;
                }

                JsonObject payload = deepClone(leftTemplate);
                replaceVariablesForLeft(payload, playerName, playerUUID, reason);
                sendWebhook(payload, leftWebhookUrl, "left");
                plugin.getLogger().info("[WebhookManager] Left webhook sent: " + playerName);
            } catch (Exception e) {
                plugin.getLogger().severe("[WebhookManager] Error sending left webhook: " + e.getMessage());
                e.printStackTrace();
            }
        });
    }

    /**
     * Változók helyettesítése JOIN webhookban
     */
    private void replaceVariablesForJoin(JsonObject obj, String playerName, UUID playerUUID) {
        replaceInObject(obj, playerName, playerUUID, null);
    }

    /**
     * Változók helyettesítése LEFT webhookban
     */
    private void replaceVariablesForLeft(JsonObject obj, String playerName, UUID playerUUID, String reason) {
        replaceInObject(obj, playerName, playerUUID, reason);
    }

    /**
     * Rekurzív string helyettesítés JsonObject-ben
     */
    private void replaceInObject(JsonObject obj, String playerName, UUID playerUUID, String reason) {
        for (String key : obj.keySet()) {
            JsonElement elem = obj.get(key);
            if (elem.isJsonPrimitive() && elem.getAsJsonPrimitive().isString()) {
                String value = elem.getAsString();
                value = value.replace("%player%", playerName);
                if (playerUUID != null) {
                    value = value.replace("%uuid%", playerUUID.toString());
                }
                if (reason != null) {
                    value = value.replace("%reason%", reason);
                }
                value = value.replace("%time%", LocalDateTime.now().format(timeFormatter));
                obj.addProperty(key, value);
            } else if (elem.isJsonArray()) {
                JsonArray arr = elem.getAsJsonArray();
                for (int i = 0; i < arr.size(); i++) {
                    JsonElement item = arr.get(i);
                    if (item.isJsonObject()) {
                        replaceInObject(item.getAsJsonObject(), playerName, playerUUID, reason);
                    } else if (item.isJsonPrimitive() && item.getAsJsonPrimitive().isString()) {
                        String value = item.getAsString();
                        value = value.replace("%player%", playerName);
                        if (playerUUID != null) {
                            value = value.replace("%uuid%", playerUUID.toString());
                        }
                        if (reason != null) {
                            value = value.replace("%reason%", reason);
                        }
                        value = value.replace("%time%", LocalDateTime.now().format(timeFormatter));
                        arr.set(i, new JsonPrimitive(value));
                    }
                }
            } else if (elem.isJsonObject()) {
                replaceInObject(elem.getAsJsonObject(), playerName, playerUUID, reason);
            }
        }
    }

    /**
     * JsonObject mély másolata
     */
    private JsonObject deepClone(JsonObject obj) {
        return JsonParser.parseString(obj.toString()).getAsJsonObject();
    }

    /**
     * Webhook üzenet küldése Discord-ra HTTP POST-tal
     */
    private void sendWebhook(JsonObject payload, String webhookUrl, String type) throws Exception {
        if (webhookUrl == null || webhookUrl.isEmpty()) {
            return;
        }

        // Validálás
        boolean hasContent = payload.has("content") && !payload.get("content").isJsonNull();
        boolean hasEmbeds = payload.has("embeds") && payload.get("embeds").isJsonArray() && payload.getAsJsonArray("embeds").size() > 0;
        boolean hasComponents = payload.has("components") && payload.get("components").isJsonArray() && payload.getAsJsonArray("components").size() > 0;

        if (!hasContent && !hasEmbeds && !hasComponents) {
            plugin.getLogger().warning("[WebhookManager] " + type + " webhook payload is invalid: missing content. Aborting send.");
            return;
        }

        HttpURLConnection connection = (HttpURLConnection) URI.create(webhookUrl).toURL().openConnection();
        connection.setRequestMethod("POST");
        connection.setRequestProperty("Content-Type", "application/json");
        connection.setDoOutput(true);
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);

        String jsonPayload = payload.toString();
        byte[] input = jsonPayload.getBytes(StandardCharsets.UTF_8);

        try (OutputStream os = connection.getOutputStream()) {
            os.write(input, 0, input.length);
        }

        int responseCode = connection.getResponseCode();
        if (responseCode != 204 && responseCode != 200) {
            plugin.getLogger().warning("[WebhookManager] " + type + " webhook hiba: HTTP " + responseCode);
        }

        connection.disconnect();
    }

    /**
     * Join webhook engedélyezve van-e?
     */
    public boolean isJoinEnabled() {
        return joinWebhookUrl != null && !joinWebhookUrl.isEmpty();
    }

    /**
     * Left webhook engedélyezve van-e?
     */
    public boolean isLeftEnabled() {
        return leftWebhookUrl != null && !leftWebhookUrl.isEmpty();
    }

    /**
     * Join webhook URL beállítása
     */
    public void setJoinWebhookUrl(String url) {
        this.joinWebhookUrl = url != null ? url : "";
    }

    /**
     * Left webhook URL beállítása
     */
    public void setLeftWebhookUrl(String url) {
        this.leftWebhookUrl = url != null ? url : "";
    }

    /**
     * Join embed típus beállítása
     */
    public void setJoinEmbedType(String type) {
        if (type != null && (type.equals("embed") || type.equals("components"))) {
            this.joinEmbedType = type;
            plugin.getLogger().info("[WebhookManager] Join embed típus módosítva: " + type);
        }
    }

    /**
     * Left embed típus beállítása
     */
    public void setLeftEmbedType(String type) {
        if (type != null && (type.equals("embed") || type.equals("components"))) {
            this.leftEmbedType = type;
            plugin.getLogger().info("[WebhookManager] Left embed típus módosítva: " + type);
        }
    }

    /**
     * Sablonok újratöltése
     */
    public void reloadTemplates() {
        loadTemplates();
    }
}
