# Honosítás Tesztelése / Localization Testing

## 🇭🇺 Magyar (Hungarian) Login Flow

### Scenario 1: Új felhasználó (nem csatolt)
**Előfeltételek:**
- Szerver alapértelmezett nyelve: `hu` (config.yml `default_lang: hu`)
- Minecraft kliens is magyarra van állítva

**Teszt lépések:**
1. Játékos megpróbál csatlakozni
2. System: Új rekord jön létre a DB-ben
3. **Várt üzenet (magyar):**
   ```
   Fiókod nincs összekötve. Kód: ABCD1234
   Érvényes: 5 percig
   ```

### Scenario 2: DB kapcsolat hiba
**Előfeltételek:**
- MySQL szerver leállt vagy elérhetetlen
- Szerver alapértelmezett nyelvé: `hu`

**Teszt lépések:**
1. Játékos megpróbál csatlakozni
2. System: LoginListener `catch (SQLException e)` blokkja aktiválódik
3. **Várt üzenet (magyar):**
   ```
   DB hiba: <SQLException message>
   ```
4. **Szerver log (DEBUG/INFO szint):**
   ```
   DB hiba belépéskor: <error details>
   ```

### Scenario 3: Elutasított felhasználó
**Előfeltételek:**
- Felhasználó már csatolt Discord fiókkal, de elutasított (`accepted = false`)
- Elutasítási indok: "Banned from server"

**Teszt lépések:**
1. Játékos megpróbál csatlakozni
2. **Várt üzenet (magyar):**
   ```
   Téged a következő indokkal utasítottak el: Banned from server
   ```

### Scenario 4: Függő felhasználó
**Előfeltételek:**
- Felhasználó csatolt Discord fiókkal, de elfogadás függőben (`accepted = null`)

**Teszt lépések:**
1. Játékos megpróbál csatlakozni
2. **Várt üzenet (magyar):**
   ```
   A jelentkezésed folyamatban van, kérlek várj.
   ```

---

## 🇺🇸 English Login Flow

### Scenario 1: New user (not linked)
**Prerequisites:**
- Server default language: `en` (change config.yml `default_lang: en`)
- Minecraft client is also set to English

**Test steps:**
1. Player attempts to join
2. System: New record created in DB
3. **Expected message (English):**
   ```
   Your account is not linked. Code: ABCD1234
   Valid for: 5 minutes
   ```

### Scenario 2: Database connection error
**Prerequisites:**
- MySQL server is down or unreachable
- Server default language: `en`

**Test steps:**
1. Player attempts to join
2. System: LoginListener `catch (SQLException e)` block activates
3. **Expected message (English):**
   ```
   Database error: <SQLException message>
   ```
4. **Server log:**
   ```
   DB hiba belépéskor: <error details>
   ```

### Scenario 3: Rejected user
**Prerequisites:**
- User already linked Discord account but rejected (`accepted = false`)
- Rejection reason: "Banned from server"

**Test steps:**
1. Player attempts to join
2. **Expected message (English):**
   ```
   Your application has been rejected for the following reason: Banned from server
   ```

### Scenario 4: Pending user
**Prerequisites:**
- User linked Discord account but acceptance pending (`accepted = null`)

**Test steps:**
1. Player attempts to join
2. **Expected message (English):**
   ```
   Your application is pending. Please wait.
   ```

---

## ✅ Implementation Details

### LangController Logic
- **Location:** `src/main/java/discordlinker/LangController.java`
- **Language Detection (Player):**
  - Reads `player.getLocale()` (e.g., "en_us", "hu_hu")
  - Converts to lang code: "en_us" → "en", "hu_hu" → "hu"
  - Falls back to `default_lang` if not available
  - **Caches result** to avoid repeated lookups
- **Language Detection (Login):**
  - Uses `default_lang` from config.yml during login (no Player object available yet)
  - After player joins, real language detected on next message lookup

### Message Files
- **Hungarian:** `src/main/resources/langs/hu_messages.yml`
- **English:** `src/main/resources/langs/en_messages.yml`
- **Auto-deployed to:** `plugins/DiscordLinker/langs/` on first plugin load

### Configuration
```yaml
# config.yml
default_lang: hu  # Change to "en" for English default
mysql:
  host: localhost
  port: 3306
  database: discord_linker
  user: root
  password: password
  table: players
```

---

## 🔍 Debugging

### Check Language Files Are Loaded
**Server Console:**
```
[INFO] [LangController] Nyelvfájl betöltve: hu
[INFO] [LangController] Nyelvfájl betöltve: en
```

### Check Player Language Detection
When player joins, LangController will automatically cache their language.
To verify, add a `/say` command with a localized message.

### Check DB Reconnect Messages
When MySQL connection fails:
```
[SEVERE] DB hiba belépéskor: Communications link failure
[INFO] Próbálok újracsatlakozni az adatbázishoz...
[INFO] Sikeres újracsatlakozás az adatbázishoz.  (or)
[SEVERE] Nem sikerült újracsatlakozni az adatbázishoz: ...
```

---

## 🚀 Next Steps

1. **Test Hungarian flow first** (default)
2. **Change `default_lang: en` in config.yml**
3. **Restart server and test English flow**
4. **Verify MySQL reconnect** by manually stopping MySQL, then restarting it
5. **Check server logs** for proper messages

