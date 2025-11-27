package discordlinker;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.Base64;

/**
 * API titkosítás — Reverzibilis titkosítási algoritmusok
 * Az adat titkosítva közvetlenül az API szerverhez, ahol ugyanazzal az API_KEY-jel dekódolható
 * 
 * Támogatott algoritmusok:
 * - "aes-256-cbc" (AES-256 CBC mód, ajánlott)
 * - "aes-128-cbc" (AES-128 CBC mód, gyorsabb de kevésbé biztos)
 * 
 * Payload formátum: salt.iv.ciphertext (Base64-kódolt)
 * - Salt: 16 random byte (PBKDF2-szerű kulcslevezetéshez)
 * - IV: 16 random byte (CBC inicializációs vektor)
 * - Ciphertext: AES titkosított JSON adat
 */
public class APIEncryption {
    private final String apiKey;
    private final String algorithm;
    private static final int SALT_LENGTH = 16;
    private static final int IV_LENGTH = 16;

    public APIEncryption(String apiKey, String algorithm) {
        this.apiKey = apiKey;
        // Támogatott: "aes-256-cbc" vagy "aes-128-cbc"
        // Egyéb értékek: fallback "aes-256-cbc"-re
        String algo = algorithm != null ? algorithm.toLowerCase() : "aes-256-cbc";
        
        if ("aes-256-cbc".equals(algo) || "aes-128-cbc".equals(algo)) {
            this.algorithm = algo;
        } else {
            // Figyelmeztetés: ismeretlen algoritmus, fallback AES-256-CBC
            this.algorithm = "aes-256-cbc";
        }
    }

    /**
     * Adatok titkosítása az adott algoritmussal
     * @param data Titkosítandó adat (pl. JSON string)
     * @return Titkosított adat formátum: salt.iv.ciphertext (Base64)
     */
    public String encrypt(String data) throws Exception {
        if (data == null || data.isEmpty()) {
            throw new IllegalArgumentException("Titkosítandó adat nem lehet üres!");
        }

        if ("aes-128-cbc".equals(algorithm)) {
            return encryptAES128CBC(data);
        } else {
            // Default: AES-256-CBC
            return encryptAES256CBC(data);
        }
    }

    /**
     * AES-256-CBC titkosítás (256-bites kulcs = 32 byte)
     * Payload formátum: salt.iv.ciphertext (Base64)
     */
    private String encryptAES256CBC(String data) throws Exception {
        byte[] salt = generateSalt();
        byte[] iv = generateIV();
        
        // Kulcs levezetése: PBKDF2-szerű (SHA-256 iterált 1024x)
        byte[] key = deriveKeyFromPassword(apiKey, salt, 32); // 256 bit = 32 byte
        
        // AES-256-CBC titkosítás
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        IvParameterSpec ivSpec = new IvParameterSpec(iv);
        SecretKeySpec keySpec = new SecretKeySpec(key, 0, key.length, "AES");
        cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec);
        
        byte[] cipherText = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        
        // Payload: salt + IV + ciphertext (Base64-kódolt)
        String saltB64 = Base64.getEncoder().encodeToString(salt);
        String ivB64 = Base64.getEncoder().encodeToString(iv);
        String cipherB64 = Base64.getEncoder().encodeToString(cipherText);
        
        return saltB64 + "." + ivB64 + "." + cipherB64;
    }

    /**
     * AES-128-CBC titkosítás (128-bites kulcs = 16 byte)
     * Payload formátum: salt.iv.ciphertext (Base64)
     * Megjegyzés: gyorsabb, de kevésbé biztos mint AES-256
     */
    private String encryptAES128CBC(String data) throws Exception {
        byte[] salt = generateSalt();
        byte[] iv = generateIV();
        
        // Kulcs levezetése: PBKDF2-szerű (SHA-256 iterált 1024x)
        byte[] key = deriveKeyFromPassword(apiKey, salt, 16); // 128 bit = 16 byte
        
        // AES-128-CBC titkosítás
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        IvParameterSpec ivSpec = new IvParameterSpec(iv);
        SecretKeySpec keySpec = new SecretKeySpec(key, 0, key.length, "AES");
        cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec);
        
        byte[] cipherText = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        
        // Payload: salt + IV + ciphertext (Base64-kódolt)
        String saltB64 = Base64.getEncoder().encodeToString(salt);
        String ivB64 = Base64.getEncoder().encodeToString(iv);
        String cipherB64 = Base64.getEncoder().encodeToString(cipherText);
        
        return saltB64 + "." + ivB64 + "." + cipherB64;
    }

    /**
     * Titkosított adat dekódolása (szerver oldali)
     * @param encrypted Titkosított adat formátum: salt.iv.ciphertext (Base64)
     * @return Eredeti adat (pl. JSON string)
     */
    public String decrypt(String encrypted) throws Exception {
        if (encrypted == null || encrypted.isEmpty()) {
            throw new IllegalArgumentException("Titkosított adat nem lehet üres!");
        }

        String[] parts = encrypted.split("\\.");
        if (parts.length != 3) {
            throw new IllegalArgumentException("Érvénytelen titkosított adat formátum! Várt: salt.iv.ciphertext");
        }

        String saltB64 = parts[0];
        String ivB64 = parts[1];
        String cipherB64 = parts[2];

        try {
            byte[] salt = Base64.getDecoder().decode(saltB64);
            byte[] iv = Base64.getDecoder().decode(ivB64);
            byte[] cipherText = Base64.getDecoder().decode(cipherB64);

            // Kulcs levezetése (ugyanaz, mint encryptionnál)
            int keyLength = "aes-128-cbc".equals(algorithm) ? 16 : 32;
            byte[] key = deriveKeyFromPassword(apiKey, salt, keyLength);

            // AES dekódolás
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            IvParameterSpec ivSpec = new IvParameterSpec(iv);
            SecretKeySpec keySpec = new SecretKeySpec(key, 0, key.length, "AES");
            cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec);

            byte[] plainText = cipher.doFinal(cipherText);
            return new String(plainText, StandardCharsets.UTF_8);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Base64 dekódolási hiba: " + e.getMessage());
        }
    }

    /**
     * Kulcs levezetése API_KEY-ből és saltból
     * PBKDF2-szerű: SHA-256 iterált 1024x az erősség miatt
     * 
     * @param password API_KEY
     * @param salt Véletlen salt (minden üzenethez más)
     * @param keyLength Szükséges kulcshossz (16 = 128 bit, 32 = 256 bit)
     * @return Levezetett kulcs
     */
    private byte[] deriveKeyFromPassword(String password, byte[] salt, int keyLength) throws Exception {
        byte[] key = new byte[keyLength];
        MessageDigest digest = MessageDigest.getInstance("SHA-256");
        
        // API_KEY + Salt kombinációja
        byte[] input = (password + Base64.getEncoder().encodeToString(salt)).getBytes(StandardCharsets.UTF_8);
        byte[] hash = digest.digest(input);
        
        // Iteráció: 1024x SHA-256 az erősség növeléséhez (PBKDF2-szerű)
        for (int i = 0; i < 1023; i++) {
            hash = digest.digest(hash);
        }
        
        // Első keyLength byte-ot használjuk
        System.arraycopy(hash, 0, key, 0, Math.min(hash.length, keyLength));
        
        return key;
    }

    /**
     * Véletlen salt generálása (16 byte)
     * Biztosítja, hogy ugyanaz az adat más-más titkosított formát kap
     */
    private byte[] generateSalt() {
        byte[] salt = new byte[SALT_LENGTH];
        new SecureRandom().nextBytes(salt);
        return salt;
    }

    /**
     * Véletlen IV (Initialization Vector) generálása
     * CBC mód szükséges a biztonsághoz (random IV minden üzenethez)
     */
    private byte[] generateIV() {
        byte[] iv = new byte[IV_LENGTH];
        new SecureRandom().nextBytes(iv);
        return iv;
    }

    /**
     * Algoritmus lekérése
     */
    public String getAlgorithm() {
        return algorithm;
    }
}



