package org.minima.database.txpowdb.onchain;

import java.io.File;
import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Date;

import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class TxPoWOnChainDB extends SqlDB {

	/**
	 * How long does data remain the SQL DB in milli seconds
	 */
	public static long MAX_ONCHAINSQL_MILLI = 1000 * 60 * 60 * 24 * 1000;
	
	/**
	 * Prepared SQL statements
	 */
	PreparedStatement SQL_INSERT_ONCHAINTXPOW 		= null;
	PreparedStatement SQL_SELECT_ONCHAINTXPOW 		= null;
	PreparedStatement SQL_DELETE_ONCHAINTXPOW 		= null;
	
	PreparedStatement SQL_SELECT_ONBLOCKTXPOW 		= null;
	PreparedStatement SQL_SELECT_ONBLOCKNUMTXPOW 	= null;
	
	PreparedStatement SQL_SELECT_FIRSTTXPOW 		= null;
	PreparedStatement SQL_SELECT_LASTTXPOW 			= null;
	
	PreparedStatement SQL_TOTAL_TXPOW 				= null;
	
	public TxPoWOnChainDB() {
		super();
	}
		
	/**
	 * Perform the Create SQL
	 * @throws SQLException 
	 */
	protected void createSQL() throws SQLException {
		
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();
			
			//Create main table
			String create = "CREATE TABLE IF NOT EXISTS `onchaintxpow` ("
							+ "  `id` bigint auto_increment,"
							+ "  `blockid` varchar(80) NOT NULL,"
							+ "  `block` bigint NOT NULL,"
							+ "  `txpowid` varchar(80) NOT NULL,"
							+ "  `timemilli` bigint NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(create);
					
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			SQL_INSERT_ONCHAINTXPOW = mSQLConnection.prepareStatement("INSERT IGNORE INTO onchaintxpow "
					+ "( blockid, block, txpowid, timemilli ) VALUES ( ?, ? ,? ,? )");
			SQL_SELECT_ONCHAINTXPOW	= mSQLConnection.prepareStatement("SELECT * FROM onchaintxpow WHERE txpowid=?");
			SQL_DELETE_ONCHAINTXPOW	= mSQLConnection.prepareStatement("DELETE FROM onchaintxpow WHERE timemilli < ?");
	
			SQL_SELECT_ONBLOCKTXPOW		= mSQLConnection.prepareStatement("SELECT * FROM onchaintxpow WHERE blockid=?");
			SQL_SELECT_ONBLOCKNUMTXPOW	= mSQLConnection.prepareStatement("SELECT * FROM onchaintxpow WHERE block=?");
			
			SQL_TOTAL_TXPOW			= mSQLConnection.prepareStatement("SELECT COUNT(*) AS tot FROM onchaintxpow");
			
			SQL_SELECT_FIRSTTXPOW 	= mSQLConnection.prepareStatement("SELECT * FROM onchaintxpow ORDER BY timemilli ASC LIMIT 1");
			SQL_SELECT_LASTTXPOW 	= mSQLConnection.prepareStatement("SELECT * FROM onchaintxpow ORDER BY timemilli DESC LIMIT 1");
	}
	
	public void wipeDB() throws SQLException {
		
		//Make sure..
		checkOpen();
		
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//First wipe everything..
		stmt.execute("DROP ALL OBJECTS");
		
		//Create main table
		String create = "CREATE TABLE IF NOT EXISTS `onchaintxpow` ("
						+ "  `id` bigint auto_increment,"
						+ "  `blockid` varchar(80) NOT NULL,"
						+ "  `block` bigint NOT NULL,"
						+ "  `txpowid` varchar(80) NOT NULL,"
						+ "  `timemilli` bigint NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//That's it..
		stmt.close();
	}
	
	public synchronized boolean addOnChainTxPoW(String zBlockID, MiniNumber zBlock, String zTxPoWID) {
		try {
			
			//Make sure..
			checkOpen();
			
			//Get the Query ready
			SQL_INSERT_ONCHAINTXPOW.clearParameters();
		
			//Set main params
			SQL_INSERT_ONCHAINTXPOW.setString(1, zBlockID);
			SQL_INSERT_ONCHAINTXPOW.setLong(2, zBlock.getAsLong());
			SQL_INSERT_ONCHAINTXPOW.setString(3, zTxPoWID);
			SQL_INSERT_ONCHAINTXPOW.setLong(4, System.currentTimeMillis());
			
			//Do it.
			SQL_INSERT_ONCHAINTXPOW.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}

	public synchronized JSONObject getOnChainTxPoW(String zTxPoWID) {
		
		//Get the blob of data
		JSONObject onchain = new JSONObject();
		onchain.put("found", false);
		onchain.put("txpowid", zTxPoWID);
		
		try {
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_ONCHAINTXPOW.clearParameters();
			
			//Set the txpowid we are searching for
			SQL_SELECT_ONCHAINTXPOW.setString(1, zTxPoWID);
		
			//Run the query
			ResultSet rs = SQL_SELECT_ONCHAINTXPOW.executeQuery();
			
			//Is there a result..
			if(rs.next()) {
				
				//Get the blob of data
				onchain.put("found", true);
				onchain.put("blockid", rs.getString("blockid"));
				onchain.put("block", rs.getLong("block"));
				onchain.put("timemilli", rs.getLong("timemilli"));
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return onchain;
	}
	
	public synchronized JSONArray getInBlockTxPoW(String zTxPoWID) {
		
		JSONArray ret = new JSONArray();
		
		try {
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_ONBLOCKTXPOW.clearParameters();
			
			//Set the txpowid we are searching for
			SQL_SELECT_ONBLOCKTXPOW.setString(1, zTxPoWID);
		
			//Run the query
			ResultSet rs = SQL_SELECT_ONBLOCKTXPOW.executeQuery();
			
			//Is there a result..
			while(rs.next()) {
				
				JSONObject onchain = new JSONObject();
				
				//Get the blob of data
				onchain.put("txpowid", rs.getString("txpowid"));
				onchain.put("blockid", rs.getString("blockid"));
				onchain.put("block", rs.getLong("block"));
				onchain.put("timemilli", rs.getLong("timemilli"));
				
				ret.add(onchain);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return ret;
	}
	
	public synchronized JSONArray getInBlockTxPoW(long zBlock) {
		
		JSONArray ret = new JSONArray();
		
		try {
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_ONBLOCKNUMTXPOW.clearParameters();
			
			//Set the txpowid we are searching for
			SQL_SELECT_ONBLOCKNUMTXPOW.setLong(1, zBlock);
		
			//Run the query
			ResultSet rs = SQL_SELECT_ONBLOCKNUMTXPOW.executeQuery();
			
			//Is there a result..
			while(rs.next()) {
				
				JSONObject onchain = new JSONObject();
				
				//Get the blob of data
				onchain.put("txpowid", rs.getString("txpowid"));
				onchain.put("blockid", rs.getString("blockid"));
				onchain.put("block", rs.getLong("block"));
				onchain.put("timemilli", rs.getLong("timemilli"));
				
				ret.add(onchain);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return ret;
	}
	
	public synchronized JSONObject getFirstTxPoW() {
		
		//Get the blob of data
		JSONObject onchain = new JSONObject();
		onchain.put("found", false);
		
		try {
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_FIRSTTXPOW.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_FIRSTTXPOW.executeQuery();
			
			//Is there a result..
			if(rs.next()) {
				
				//Get the blob of data
				onchain.put("found", true);
				onchain.put("txpowid", rs.getString("txpowid"));
				onchain.put("blockid", rs.getString("blockid"));
				onchain.put("block", rs.getLong("block"));
				
				long tm = rs.getLong("timemilli");
				onchain.put("timemilli", tm);
				onchain.put("date", new Date(tm).toString());
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return onchain;
	}
	
	public synchronized JSONObject getLastTxPoW() {
		
		//Get the blob of data
		JSONObject onchain = new JSONObject();
		onchain.put("found", false);
		
		try {
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_LASTTXPOW.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_LASTTXPOW.executeQuery();
			
			//Is there a result..
			if(rs.next()) {
				
				//Get the blob of data
				onchain.put("found", true);
				onchain.put("txpowid", rs.getString("txpowid"));
				onchain.put("blockid", rs.getString("blockid"));
				onchain.put("block", rs.getLong("block"));
				
				long tm = rs.getLong("timemilli");
				onchain.put("timemilli", tm);
				onchain.put("date", new Date(tm).toString());
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return onchain;
	}
	
	
	
	public synchronized int getSize() {
		try {
			//Make sure..
			checkOpen();
			
			//Run the query
			ResultSet rs = SQL_TOTAL_TXPOW.executeQuery();
			
			//Could be multiple results
			if(rs.next()) {
				//Get the total numer of rows
				return rs.getInt("tot");
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		//Error has occurred
		return -1;
	}
	
	
	/**
	 * Returns how many rows were deleted
	 */
	public synchronized int cleanDB() {
		return cleanDB(false);
	}
	
	public synchronized int cleanDB(boolean zHard) {
		
		//For NOW do not delete!.
		if(true) {
			return 0;
		}
		
		try {
			
			//Make sure..
			checkOpen();
			
			//Current MAX time..
			long maxtime = System.currentTimeMillis() - MAX_ONCHAINSQL_MILLI;
			if(zHard) {
				maxtime = System.currentTimeMillis() + 100000;
			}
			
			//Set the parameters
			SQL_DELETE_ONCHAINTXPOW.clearParameters();
			
			//Set the time milli
			SQL_DELETE_ONCHAINTXPOW.setLong(1, maxtime);
			
			//Run the query
			int num = SQL_DELETE_ONCHAINTXPOW.executeUpdate();
			
			return num;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return 0;
	}
	
	public static void main(String[] zArgs) throws IOException, SQLException {
		
		File testdb = new File(System.getProperty("user.home"),"testsql");
		
		TxPoWOnChainDB db = new TxPoWOnChainDB();
		db.loadDB(testdb);
		
		db.createSQL();

		db.addOnChainTxPoW("0xFF", MiniNumber.ONE, "txPowID-01");
		db.addOnChainTxPoW("0xEE", MiniNumber.TWO, "txPowID-02");
		db.addOnChainTxPoW("0xDD", MiniNumber.THREE, "txPowID-03");
		
		//Now load..
		System.out.println(db.getOnChainTxPoW("txPowID-01").toString());
		System.out.println(db.getOnChainTxPoW("txPowID-02").toString());
	}
}
