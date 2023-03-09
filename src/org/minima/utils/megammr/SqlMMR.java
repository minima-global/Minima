package org.minima.utils.megammr;

import org.minima.database.mmr.*;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class SqlMMR {
    static final int MAX_SYNCBLOCKS = 250;
    static final String SELECT_BLOCK_RANGE = "SELECT syncdata FROM syncblock WHERE block>=? ORDER BY block ASC LIMIT ";
    static final String MMR_TABLE = "CREATE TABLE IF NOT EXISTS `mmr` ("
            + " `id` VARCHAR(20) NOT NULL PRIMARY KEY,"
            + " `row` INT NOT NULL,"
            + " `entry` INT NOT NULL,"
            + " `value` INT NOT NULL,"
            + " `data` MEDIUMBLOB NOT NULL"
            + ")";
    static final String INSERT_MMR = "INSERT INTO `mmr`" +
            " (`id`, `row`, `entry`, `value`, `data` ) "
            + "VALUES (? ,?, ?, ?, ?) "
            + "ON DUPLICATE KEY "
            + "UPDATE `row`=VALUES(`row`), `entry`=VALUES(`entry`), `value`=VALUES(`value`), `data`=VALUES(`data`)";
    static final String COIN_TABLE = "CREATE TABLE IF NOT EXISTS `coin` ("
            + " `mmr_entry` VARCHAR(20) NOT NULL PRIMARY KEY,"
            + " `id` VARCHAR(80) NOT NULL,"
            + " `address` VARCHAR(80) NOT NULL,"
            + " `amount` INT NOT NULL,"
            + " `spent` BOOLEAN NOT NULL"
            + ")";
    static final String INSERT_COIN = "INSERT INTO `coin`" +
            " (`mmr_entry`, `id`, `address`, `amount`, `spent` ) "
            + "VALUES (?, ?, ?, ?, ?) "
            + "ON DUPLICATE KEY "
            + "UPDATE `id`=VALUES(`id`), `address`=VALUES(`address`), `amount`=VALUES(`amount`), `spent`=VALUES(`spent`)";
    private static final String FIND_MMR = "SELECT `data`, `value` from `mmr` where `id`=?";
    private static final MMREntry emptyMMR = new MMREntry(-1, new MMREntryNumber(-1));

    public static void main(String[] args) {
        String mMySQLHost = "localhost:3307";
        String mDatabase = "archivedb";
        String username = "archiveuser";
        String password = "archivepassword";

        final String mysqldb = "jdbc:mysql://" + mMySQLHost + "/" + mDatabase
                + "?autoReconnect=true"
                + "&logger=com.mysql.cj.log.StandardLogger&profileSQL=true";
        try (Connection connection = DriverManager.getConnection(mysqldb, username, password)) {
//            syncMMR(connection);
//            MinimaLogger.log("entry: " + getEntry(connection, 0, new MMREntryNumber(100)));
            MinimaLogger.log("proof: " + proofToPeak(connection, 0, new MMREntryNumber(100)));
        } catch (SQLException e) {
            MinimaLogger.log(e);
        }
    }

    private static MMRProof proofToPeak(Connection conn, int row, MMREntryNumber number) {
        final MMRProof proof = new MMRProof();

        final MMREntry entry = getEntry(conn, row, number);
        if (entry.isEmpty()) {
            MinimaLogger.log(
                    String.format("entry not found while calculating proof, row[%s] number[%s]", row, number)
            );
            return proof;
        }

        MMREntry sibling = getEntry(conn, entry.getRow(), entry.getSibling());
        while (!sibling.isEmpty()) {
            //Add to our Proof..
            proof.addProofChunk(sibling.isLeft(), sibling.getMMRData());

            //Now get the Parent.. just need a reference even if is empty. To find the sibling.
            MMREntry parent = new MMREntry(sibling.getParentRow(), sibling.getParentEntry());

            //And get the Sibling of the Parent..
            sibling = getEntry(conn, parent.getRow(), parent.getSibling());
        }

        return proof;
    }

    static MMREntry getEntry(Connection conn, int row, MMREntryNumber number) {
        try (final PreparedStatement statement = conn.prepareStatement(FIND_MMR)) {
            statement.setString(1, row + ":" + number.getBigDecimal().intValue());
            final ResultSet resultSet = statement.executeQuery();
            if (resultSet.next()) {
                MMRData data = new MMRData(
                        new MiniData(resultSet.getBytes(1)),
                        new MiniNumber(resultSet.getLong(2))
                );
                return new MMREntry(row, number, data);
            }
        } catch (SQLException e) {
            MinimaLogger.log(e);
        }
        return emptyMMR;
    }

    private static void syncMMR(Connection connection) {
        prepareDB(connection);

        // start reading
        MiniNumber startblock = MiniNumber.ZERO;
        while (true) {
            final List<TxBlock> blocks = loadBlockRange(connection, startblock);
            if (blocks.isEmpty()) {
                break;
            }
            for (TxBlock block : blocks) {
                processBlock(connection, block);
                startblock = block.getTxPoW().getBlockNumber().increment();
            }
        }
    }

    static void prepareDB(Connection connection) {
        try (Statement stmt = connection.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS `mmr`");
            stmt.execute(MMR_TABLE);
            stmt.execute("DROP TABLE IF EXISTS `coin`");
            stmt.execute(COIN_TABLE);
        } catch (SQLException e) {
            MinimaLogger.log(e);
        }
    }

    private static void processBlock(Connection conn, TxBlock block) {
        MinimaLogger.log("block:" + block.getTxPoW().getBlockNumber() + " peaks:" + block.getPreviousPeaks());
        final TxPoWTreeNode node = new TxPoWTreeNode(block, false);
        processMMR(conn, node.getMMR());
        processCoins(conn, node.getAllCoins());
    }

    private static void processCoins(Connection conn, ArrayList<Coin> coins) {
        try (final PreparedStatement statement = conn.prepareStatement(INSERT_COIN)) {
            coins.forEach(coin -> {
                try {
                    statement.clearParameters();
                    statement.setString(1, "0:" + coin.getMMREntryNumber());
                    statement.setString(2, coin.getCoinID().to0xString());
                    statement.setString(3, coin.getAddress().to0xString());
                    statement.setInt(4, coin.getAmount().getAsInt());
                    statement.setBoolean(5, coin.getSpent());
                    statement.executeUpdate();
                } catch (SQLException e) {
                    MinimaLogger.log(e);
                }
            });
        } catch (SQLException e) {
            MinimaLogger.log(e);
        }
    }

    private static void processMMR(Connection conn, MMR mmr) {
        try (final PreparedStatement statement = conn.prepareStatement(INSERT_MMR)) {
            mmr.getAllEntries().forEach((key, value) -> {
                try {
                    statement.clearParameters();
                    statement.setString(1, key);
                    statement.setInt(2, value.getRow());
                    statement.setInt(3, Integer.parseInt(value.getEntryNumber().toString()));
                    statement.setInt(4, value.getMMRData().getValue().getAsInt());
                    statement.setBytes(5, value.getMMRData().getData().getBytes());
                    statement.executeUpdate();
                } catch (SQLException e) {
                    MinimaLogger.log(e);
                }
            });
        } catch (SQLException e) {
            MinimaLogger.log(e);
        }

        MinimaLogger.log(" mmr:" + mmr.getEntryNumber() + ", " +
                mmr.getAllEntries().entrySet().stream()
                        .map(entry -> entry.getKey() + " value:" + entry.getValue().getMMRData().getValue())
                        .collect(Collectors.joining(", ")));

    }

    public static List<TxBlock> loadBlockRange(Connection mConnection, MiniNumber zStartBlock) {
        final ArrayList<TxBlock> blocks = new ArrayList<>();

        try (final PreparedStatement sqlSelectRange = mConnection.prepareStatement(SELECT_BLOCK_RANGE + MAX_SYNCBLOCKS)) {
            //Set Search params
            sqlSelectRange.clearParameters();
            sqlSelectRange.setLong(1, zStartBlock.getAsLong());
            //Run the query
            final ResultSet rs = sqlSelectRange.executeQuery();
            while (rs.next()) {
                byte[] syncdata = rs.getBytes("syncdata");
                blocks.add(
                        TxBlock.convertMiniDataVersion(new MiniData(syncdata))
                );
            }
        } catch (SQLException e) {
            MinimaLogger.log(e);
        }
        return blocks;
    }
}
