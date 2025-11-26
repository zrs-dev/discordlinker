package discordlinker;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import java.sql.*;

public class MySQLManager {
    private HikariDataSource ds;
    private final String tableName;

    // store params so we can recreate the pool on reconnect
    private final String host;
    private final int port;
    private final String database;
    private final String user;
    private final String password;

    public MySQLManager(String host, int port, String database, String user, String password, String tableName) throws SQLException {
        this.tableName = tableName;
        this.host = host;
        this.port = port;
        this.database = database;
        this.user = user;
        this.password = password;

        createDataSource();

        try (Connection c = getConnectionWithRetry()) {
            // tesztkapcsolat
        }
    }

    public void close() {
        if (ds != null && !ds.isClosed()) ds.close();
    }

    private void createDataSource() {
        HikariConfig cfg = new HikariConfig();
        cfg.setJdbcUrl("jdbc:mysql://" + host + ":" + port + "/" + database + "?useSSL=false&serverTimezone=UTC");
        cfg.setUsername(user);
        cfg.setPassword(password);

        // Pool sizing and timeouts
        cfg.setMaximumPoolSize(4);
        cfg.setMinimumIdle(1);
        cfg.setConnectionTimeout(5000);
        // recommended to keep maxLifetime shorter than MySQL wait_timeout
        cfg.setMaxLifetime(30 * 60 * 1000); // 30 minutes
        cfg.setIdleTimeout(10 * 60 * 1000); // 10 minutes

        // validation
        cfg.setConnectionTestQuery("SELECT 1");
        cfg.setValidationTimeout(3000);

        // helpful properties
        cfg.addDataSourceProperty("cachePrepStmts", "true");
        cfg.addDataSourceProperty("useServerPrepStmts", "true");
        cfg.addDataSourceProperty("prepStmtCacheSize", "250");
        cfg.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

        // create datasource
        if (ds != null && !ds.isClosed()) {
            try {
                ds.close();
            } catch (Exception ignored) {}
        }
        ds = new HikariDataSource(cfg);
    }

    /**
     * Attempt to recreate the datasource (synchronized)
     */
    private synchronized void reconnect() {
        try {
            createDataSource();
            // test
            try (Connection c = ds.getConnection()) {
                // success
            }
        } catch (Exception e) {
            // log using System.err because plugin logger is not available here
            System.err.println("[DiscordLinker] Failed to reconnect to DB: " + e.getMessage());
        }
    }

    /**
     * Get a connection, and retry once after attempting reconnect if obtaining fails.
     */
    private Connection getConnectionWithRetry() throws SQLException {
        try {
            return ds.getConnection();
        } catch (SQLException firstEx) {
            // try reconnect and one more attempt
            reconnect();
            try {
                return ds.getConnection();
            } catch (SQLException secondEx) {
                // throw the second exception for accuracy
                throw secondEx;
            }
        }
    }

    public void createTableIfNotExists() throws SQLException {
        String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (\n" +
                "  mc_name VARCHAR(32) PRIMARY KEY,\n" +
                "  mc_uuid CHAR(36) NOT NULL,\n" +
                "  generated_code VARCHAR(64) NOT NULL,\n" +
                "  created_at DATETIME NOT NULL,\n" +
                "  expiration_at DATETIME NOT NULL,\n" +
                "  accepted TINYINT(1) DEFAULT NULL,\n" +
                "  discord_userid VARCHAR(64) DEFAULT NULL,\n" +
                "  accepted_by VARCHAR(64) DEFAULT NULL,\n" +
                "  accepted_at DATETIME DEFAULT NULL,\n" +
                "  reject_reason VARCHAR(255) DEFAULT NULL,\n" +
                "  INDEX (mc_uuid),\n" +
                "  INDEX (generated_code)\n" +
                ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
        try (Connection c = ds.getConnection(); Statement s = c.createStatement()) {
            s.execute(sql);
        }
    }

    public static class Record {
        public final String mcName;
        public final String mcUuid;
        public final String generatedCode;
        public final Timestamp createdAt;
        public final Timestamp expirationAt;
        public final Boolean accepted; // NULL = pending, TRUE = accepted, FALSE = rejected
        public final String discordUserId;
        public final String acceptedBy;
        public final Timestamp acceptedAt;
        public final String rejectReason;

        public Record(String mcName, String mcUuid, String generatedCode, Timestamp createdAt, Timestamp expirationAt,
                      Boolean accepted, String discordUserId, String acceptedBy, Timestamp acceptedAt, String rejectReason) {
            this.mcName = mcName;
            this.mcUuid = mcUuid;
            this.generatedCode = generatedCode;
            this.createdAt = createdAt;
            this.expirationAt = expirationAt;
            this.accepted = accepted;
            this.discordUserId = discordUserId;
            this.acceptedBy = acceptedBy;
            this.acceptedAt = acceptedAt;
            this.rejectReason = rejectReason;
        }
    }

    public Record getRecord(String mcName) throws SQLException {
        String q = "SELECT * FROM `" + tableName + "` WHERE mc_name = ?";
        try (Connection c = ds.getConnection(); PreparedStatement ps = c.prepareStatement(q)) {
            ps.setString(1, mcName);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) return null;
                String mcUuid = rs.getString("mc_uuid");
                String code = rs.getString("generated_code");
                Timestamp created = rs.getTimestamp("created_at");
                Timestamp exp = rs.getTimestamp("expiration_at");
                Boolean accepted = rs.getObject("accepted") == null ? null : rs.getBoolean("accepted");
                String discordId = rs.getString("discord_userid");
                String acceptedBy = rs.getString("accepted_by");
                Timestamp acceptedAt = rs.getTimestamp("accepted_at");
                String rejectReason = rs.getString("reject_reason");
                return new Record(mcName, mcUuid, code, created, exp, accepted, discordId, acceptedBy, acceptedAt, rejectReason);
            }
        }
    }

    public void insertNewCode(String mcName, String uuid, String code, Timestamp created, Timestamp expires) throws SQLException {
        String ins = "INSERT INTO `" + tableName + "` (mc_name, mc_uuid, generated_code, created_at, expiration_at, accepted, discord_userid, reject_reason) " +
                "VALUES (?, ?, ?, ?, ?, NULL, NULL, NULL) " +
                "ON DUPLICATE KEY UPDATE mc_uuid = VALUES(mc_uuid), generated_code = VALUES(generated_code), created_at = VALUES(created_at), expiration_at = VALUES(expiration_at), accepted = NULL, discord_userid = NULL, reject_reason = NULL";
        try (Connection c = ds.getConnection(); PreparedStatement ps = c.prepareStatement(ins)) {
            ps.setString(1, mcName);
            ps.setString(2, uuid);
            ps.setString(3, code);
            ps.setTimestamp(4, created);
            ps.setTimestamp(5, expires);
            ps.executeUpdate();
        }
    }

    public boolean updateCodeIfNotLinked(String mcName, String uuid, String code, Timestamp created, Timestamp expires) throws SQLException {
        String q = "UPDATE `" + tableName + "` SET generated_code = ?, created_at = ?, expiration_at = ?, mc_uuid = ? " +
                "WHERE mc_name = ? AND discord_userid IS NULL";
        try (Connection c = ds.getConnection(); PreparedStatement ps = c.prepareStatement(q)) {
            ps.setString(1, code);
            ps.setTimestamp(2, created);
            ps.setTimestamp(3, expires);
            ps.setString(4, uuid);
            ps.setString(5, mcName);
            int updated = ps.executeUpdate();
            return updated > 0;
        }
    }

    public void acceptRequest(String mcName, String discordUserId, String acceptedBy) throws SQLException {
        String q = "UPDATE `" + tableName + "` SET accepted = 1, discord_userid = ?, accepted_by = ?, accepted_at = NOW(), reject_reason = NULL WHERE mc_name = ?";
        try (Connection c = ds.getConnection(); PreparedStatement ps = c.prepareStatement(q)) {
            ps.setString(1, discordUserId);
            ps.setString(2, acceptedBy);
            ps.setString(3, mcName);
            ps.executeUpdate();
        }
    }

    public void rejectRequest(String mcName, String rejectReason, String rejectedBy) throws SQLException {
        String q = "UPDATE `" + tableName + "` SET accepted = 0, reject_reason = ?, accepted_by = ?, accepted_at = NOW() WHERE mc_name = ?";
        try (Connection c = ds.getConnection(); PreparedStatement ps = c.prepareStatement(q)) {
            ps.setString(1, rejectReason);
            ps.setString(2, rejectedBy);
            ps.setString(3, mcName);
            ps.executeUpdate();
        }
    }

    public void unlink(String mcName) throws SQLException {
        String q = "UPDATE `" + tableName + "` SET discord_userid = NULL, accepted = NULL, accepted_by = NULL, accepted_at = NULL, reject_reason = NULL WHERE mc_name = ?";
        try (Connection c = ds.getConnection(); PreparedStatement ps = c.prepareStatement(q)) {
            ps.setString(1, mcName);
            ps.executeUpdate();
        }
    }
}
