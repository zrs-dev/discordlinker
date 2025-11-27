// apiServer.js
const express = require('express');
const crypto = require('crypto');

const BASE_API_KEY = "your_secret_api_key_here_minimum_32_chars";
const PORT = 65531;

module.exports.init = function(client) {
    const app = express();
    app.use(express.text({ type: '*/*', limit: '1mb' }));

    app.post('/api/minecraft/joinrequest', (req, res) => {
        const encryptedPayload = req.body;
        console.log("=== NEW REQUEST ===");
        console.log("Raw payload:", encryptedPayload);

        try {
            const [saltB64, ivB64, cipherB64] = encryptedPayload.split('.');
            console.log("Salt (B64):", saltB64);
            console.log("IV (B64):", ivB64);
            console.log("Ciphertext (B64):", cipherB64);

            const salt = Buffer.from(saltB64, 'base64');
            const iv = Buffer.from(ivB64, 'base64');
            const ciphertext = Buffer.from(cipherB64, 'base64');

            console.log("Salt length:", salt.length);
            console.log("IV length:", iv.length);
            console.log("Ciphertext length:", ciphertext.length);

            if (iv.length !== 16) throw new Error("IV must be 16 bytes");

            // 🔑 Kulcs-levezetés (JAVÁ-val kompatibilis)
            // Java uses: SHA256( API_KEY + Base64(salt) ), then 1023 additional SHA256 iterations
            let hash = crypto.createHash('sha256')
                .update(Buffer.from(BASE_API_KEY + saltB64, 'utf8'))
                .digest();
            console.log("Key derivation start - first hash:", hash.toString('hex'));

            for (let i = 0; i < 1023; i++) {
                hash = crypto.createHash('sha256').update(hash).digest();
                if (i === 0 || i === 1022) {
                    console.log(`Hash iteration ${i + 1}:`, hash.toString('hex'));
                }
            }

            const key = hash.slice(0, 32); // use first 32 bytes for AES-256
            console.log("Derived key (32 bytes):", key.toString('hex'));

            const decipher = crypto.createDecipheriv('aes-256-cbc', key, iv);
            const decrypted = Buffer.concat([decipher.update(ciphertext), decipher.final()]);
            console.log("Decrypted buffer length:", decrypted.length);

            const originalData = decrypted.toString('utf-8');
            console.log("Original data string:", originalData);

            let jsonData;
            try {
                jsonData = JSON.parse(originalData);
                console.log("Parsed JSON:", jsonData);
            } catch (parseErr) {
                console.error("JSON parse error:", parseErr);
                jsonData = {};
            }

            res.json({
                status: "success",
                decoded: true,
                username: jsonData.username || null,
                uuid: jsonData.uuid || null
            });

        } catch (error) {
            console.error("Decrypt error:", error);
            res.status(400).json({
                status: "error",
                message: "Dekódolás sikertelen: " + error.message,
                stack: error.stack
            });
        }
    });

    app.use((req, res) => res.status(403).json({ error: "Forbidden" }));

    const server = app.listen(PORT, () => {
        console.log(`[API] Server listening on port ${PORT}`);
        if (client && client.user) console.log(`[API] init called from Discord client: ${client.user.tag}`);
    });

    return server;
};
