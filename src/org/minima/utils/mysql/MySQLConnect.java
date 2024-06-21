package org.minima.utils.mysql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.database.cascade.Cascade;
import org.minima.objects.Coin;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MySQLConnect {

	public static final int MAX_SYNCBLOCKS = 250; 
	
	String mMySQLHost;
	String mDatabase;
	String mUsername;
	String mPassword;
	
	Connection mConnection;
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_SYNCBLOCK 		= null;
	PreparedStatement SQL_FIND_SYNCBLOCK_ID 	= null;
	PreparedStatement SQL_FIND_SYNCBLOCK_NUM 	= null;
	PreparedStatement SQL_SELECT_RANGE			= null;
	
	PreparedStatement SQL_SELECT_LAST_BLOCK		= null;
	PreparedStatement SQL_SELECT_FIRST_BLOCK	= null;
	
	PreparedStatement SQL_COUNT					= null;
	
	PreparedStatement SAVE_CASCADE				= null;
	PreparedStatement LOAD_CASCADE				= null;
	
	PreparedStatement SQL_INSERT_COIN			= null;
	PreparedStatement SQL_LATEST_COIN			= null;
	PreparedStatement SQL_TOTAL_COIN			= null;
	
	PreparedStatement SQL_INSERT_TXPOW 			= null;
	PreparedStatement SQL_GET_TXPOW 			= null;
	
	boolean mReadOnly;
	
	public MySQLConnect(String zHost, String zDatabase, String zUsername, String zPassword) {
		this(zHost, zDatabase, zUsername, zPassword, false);
	}
	
	public MySQLConnect(String zHost, String zDatabase, String zUsername, String zPassword, boolean zReadOnly) {
		mMySQLHost 	= zHost;
		mDatabase	= zDatabase;
		mUsername	= zUsername;
		mPassword	= zPassword;
		mReadOnly	= zReadOnly;
	}
	
	public void init() throws SQLException {
		//MYSQL JDBC connection
		String mysqldb = "jdbc:mysql://"+mMySQLHost+"/"+mDatabase+"?autoReconnect=true";
				
		mConnection = DriverManager.getConnection(mysqldb,mUsername,mPassword);
	
		//Read only mode doesn't create the DB
		if(!mReadOnly) {
			Statement stmt = mConnection.createStatement();
			
			//Create a new DB
			String create = "CREATE TABLE IF NOT EXISTS `syncblock` ("
							+ "  `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,"
							+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
							+ "  `block` bigint NOT NULL UNIQUE,"
							+ "  `timemilli` bigint NOT NULL,"
							+ "  `syncdata` mediumblob NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(create);
			
			//Create the cascade table
			String cascade = "CREATE TABLE IF NOT EXISTS `cascadedata` ("
							+ "		`id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,"
							+ "		`cascadetip` BIGINT NOT NULL,"
							+ "		`fulldata` mediumblob NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(cascade);
			
			//Create the coins table
			String coins = "CREATE TABLE IF NOT EXISTS `coins` ("
					+ "	 `id` bigint NOT NULL AUTO_INCREMENT PRIMARY KEY,"
					+ "  `coinid` varchar(128) NOT NULL,"
					+ "  `address` varchar(128) NOT NULL,"
					+ "  `amount` varchar(128) NOT NULL,"
					+ "  `amountdouble` double NOT NULL,"
					+ "  `tokenid` varchar(128) NOT NULL,"
					+ "  `storestate` int NOT NULL,"
					+ "  `state` text,"
					+ "  `mmrentrynumber` bigint NOT NULL,"
					+ "  `spent` int NOT NULL,"
					+ "  `blockcreated` bigint NOT NULL,"
					+ "  `blockspent` bigint NOT NULL,"
					+ "  `date` varchar(128) NOT NULL,"
					+ "  `token` text,"
					+ "  `tokenamount` double NOT NULL"
					+ ")";
			
			//Run it..
			stmt.execute(coins);
			
			//Create a complete TxPoW table
			String txpow = "CREATE TABLE IF NOT EXISTS `txpow` ("
							+ "  `id` bigint NOT NULL AUTO_INCREMENT PRIMARY KEY,"
							+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
							+ "  `txpowdata` mediumblob NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(txpow);
			
			//All done..
			stmt.close();
		}
		
		//Create some prepared statements..
		String insert 			= "INSERT IGNORE INTO syncblock ( txpowid, block, timemilli, syncdata ) VALUES ( ?, ? ,? ,? )";
		SQL_INSERT_SYNCBLOCK 	= mConnection.prepareStatement(insert);
		SQL_FIND_SYNCBLOCK_ID 	= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
		SQL_FIND_SYNCBLOCK_NUM 	= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block=?");
		SQL_SELECT_RANGE		= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>=? ORDER BY block ASC LIMIT "+MAX_SYNCBLOCKS);
		
		SQL_SELECT_LAST_BLOCK	= mConnection.prepareStatement("SELECT block FROM syncblock ORDER BY block ASC LIMIT 1");
		SQL_SELECT_FIRST_BLOCK	= mConnection.prepareStatement("SELECT block FROM syncblock ORDER BY block DESC LIMIT 1");
		
		SQL_COUNT				= mConnection.prepareStatement("SELECT Count(*) as tot FROM syncblock");
		
		SAVE_CASCADE = mConnection.prepareStatement("INSERT INTO cascadedata ( cascadetip, fulldata ) VALUES ( ?, ? )");
		LOAD_CASCADE = mConnection.prepareStatement("SELECT fulldata FROM cascadedata ORDER BY cascadetip ASC LIMIT 1");
	
		SQL_INSERT_COIN = mConnection.prepareStatement("INSERT INTO coins(coinid,address,amount,amountdouble,tokenid,storestate,state,mmrentrynumber,spent,blockcreated,blockspent,date,token,tokenamount) "
					+ "VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )");
		SQL_LATEST_COIN = mConnection.prepareStatement("SELECT MAX(blockcreated) as maxblock from coins");
		SQL_TOTAL_COIN = mConnection.prepareStatement("SELECT COUNT(*) as tot from coins");
	
		SQL_INSERT_TXPOW 	= mConnection.prepareStatement("INSERT IGNORE INTO txpow ( txpowid, txpowdata ) VALUES ( ?, ? )");
		SQL_GET_TXPOW 		= mConnection.prepareStatement("SELECT * FROM txpow WHERE txpowid=?");
	}
	
	public void shutdown() {
		try {
			if(!mConnection.isClosed()) {
				mConnection.close();
			}
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void wipeAll() throws SQLException {
		Statement stmt = mConnection.createStatement();
		
		stmt.execute("DROP TABLE syncblock");
		stmt.execute("DROP TABLE cascadedata");
		stmt.execute("DROP TABLE coins");
		stmt.execute("DROP TABLE txpow");
		
		stmt.close();
		
		//close..
		shutdown();
		
		//And restart
		init();
	}
	
	public void wipeCoinsDB() throws SQLException {
		Statement stmt = mConnection.createStatement();
		stmt.execute("DROP TABLE coins");
		stmt.close();
		
		//close..
		shutdown();
		
		//And restart
		init();
	}
	
	public boolean saveCascade(Cascade zCascade) throws SQLException {
			
		//get the MiniData version..
		MiniData cascdata = MiniData.getMiniDataVersion(zCascade);
		
		//Get the Query ready
		SAVE_CASCADE.clearParameters();
	
		//Set main params
		SAVE_CASCADE.setLong(1, zCascade.getTip().getTxPoW().getBlockNumber().getAsLong());
		
		//And finally the actual bytes
		SAVE_CASCADE.setBytes(2, cascdata.getBytes());
		
		//Do it.
		SAVE_CASCADE.execute();
		
		return true;
	}
	
	
	public Cascade loadCascade() throws SQLException {
		
		LOAD_CASCADE.clearParameters();
		
		ResultSet rs = LOAD_CASCADE.executeQuery();
		
		//Is there a valid result.. ?
		if(rs.next()) {
			
			//Get the details..
			byte[] syncdata 	= rs.getBytes("fulldata");
			
			//Create MiniData version
			MiniData minisync = new MiniData(syncdata);
			
			//Convert
			Cascade casc = Cascade.convertMiniDataVersion(minisync);
			
			return casc;
		}
		
		return null;
	}
	
	public synchronized int getCount() throws SQLException {
		
		try {
			
			//Run the query
			ResultSet rs = SQL_COUNT.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the total count..
				int total = rs.getInt("tot");
				
				return total;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return -1;
	}
	
	public synchronized boolean saveBlock(TxBlock zBlock) {
		try {
			
			//get the MiniData version..
			MiniData syncdata = MiniData.getMiniDataVersion(zBlock);
			
			//Get the Query ready
			SQL_INSERT_SYNCBLOCK.clearParameters();
		
			//Set main params
			SQL_INSERT_SYNCBLOCK.setString(1, zBlock.getTxPoW().getTxPoWID());
			SQL_INSERT_SYNCBLOCK.setLong(2, zBlock.getTxPoW().getBlockNumber().getAsLong());
			SQL_INSERT_SYNCBLOCK.setLong(3, System.currentTimeMillis());
			
			//And finally the actual bytes
			SQL_INSERT_SYNCBLOCK.setBytes(4, syncdata.getBytes());
			
			//Do it.
			SQL_INSERT_SYNCBLOCK.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized TxBlock loadBlockFromID(String zTxPoWID) {
		
		try {
			
			//Set search params
			SQL_FIND_SYNCBLOCK_ID.clearParameters();
			SQL_FIND_SYNCBLOCK_ID.setString(1, zTxPoWID);
			
			//Run the query
			ResultSet rs = SQL_FIND_SYNCBLOCK_ID.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the details..
				byte[] syncdata 	= rs.getBytes("syncdata");
				
				//Create MiniData version
				MiniData minisync = new MiniData(syncdata);
				
				//Convert
				TxBlock sb = TxBlock.convertMiniDataVersion(minisync);
				
				return sb;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	public synchronized TxBlock loadBlockFromNum(long zBlocknumber) {
		
		try {
			
			//Set search params
			SQL_FIND_SYNCBLOCK_NUM.clearParameters();
			SQL_FIND_SYNCBLOCK_NUM.setLong(1, zBlocknumber);
			
			//Run the query
			ResultSet rs = SQL_FIND_SYNCBLOCK_NUM.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the details..
				byte[] syncdata 	= rs.getBytes("syncdata");
				
				//Create MiniData version
				MiniData minisync = new MiniData(syncdata);
				
				//Convert
				TxBlock sb = TxBlock.convertMiniDataVersion(minisync);
				
				return sb;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}

	public synchronized long loadFirstBlock() {
		
		try {
			
			//Set search params
			SQL_SELECT_FIRST_BLOCK.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_FIRST_BLOCK.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the block
				long block = rs.getLong("block");
				
				return block;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return -1;
	}
	
	public synchronized long loadLastBlock() {
		
		try {
			
			//Set search params
			SQL_SELECT_LAST_BLOCK.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_LAST_BLOCK.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the block
				long block = rs.getLong("block");
				
				return block;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return -1;
	}
	
	public synchronized ArrayList<TxBlock> loadBlockRange(MiniNumber zStartBlock) {
		
		ArrayList<TxBlock> blocks = new ArrayList<>();
		
		try {
			
			//Set Search params
			SQL_SELECT_RANGE.clearParameters();
			SQL_SELECT_RANGE.setLong(1,zStartBlock.getAsLong());
			
			//Run the query
			ResultSet rs = SQL_SELECT_RANGE.executeQuery();
			
			//Multiple results
			while(rs.next()) {
				
				//Get the details..
				byte[] syncdata 	= rs.getBytes("syncdata");
				
				//Create MiniData version
				MiniData minisync = new MiniData(syncdata);
				
				//Convert
				TxBlock sb = TxBlock.convertMiniDataVersion(minisync);
				
				//Add to our list
				blocks.add(sb);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return blocks;
	}
	
	public synchronized void insertCoin(Coin zCoin, long zBlockSpent, String zDate) {
		
		try {
			
			//Set Search params
			SQL_INSERT_COIN.clearParameters();
			SQL_INSERT_COIN.setString(1,zCoin.getCoinID().to0xString());
			SQL_INSERT_COIN.setString(2,zCoin.getAddress().to0xString());
			SQL_INSERT_COIN.setString(3,zCoin.getAmount().toString());
			SQL_INSERT_COIN.setDouble(4,zCoin.getAmount().getAsDouble());
			SQL_INSERT_COIN.setString(5,zCoin.getTokenID().to0xString());
			
			if(zCoin.storeState()) {
				SQL_INSERT_COIN.setInt(6,1);
			}else {
				SQL_INSERT_COIN.setInt(6,0);
			}
			
			SQL_INSERT_COIN.setString(7,zCoin.getStateAsJSON().toString());
			SQL_INSERT_COIN.setLong(8,zCoin.getMMREntryNumber().getBigDecimal().longValue());
			
			if(zCoin.getSpent()) {
				SQL_INSERT_COIN.setInt(9,1);
			}else {
				SQL_INSERT_COIN.setInt(9,0);
			}
			
			SQL_INSERT_COIN.setLong(10,zCoin.getBlockCreated().getAsLong());
			SQL_INSERT_COIN.setLong(11,zBlockSpent);
			SQL_INSERT_COIN.setString(12,zDate);
			
			if(zCoin.getToken() == null) {
				SQL_INSERT_COIN.setString(13,"");
			}else {
				SQL_INSERT_COIN.setString(13,zCoin.getToken().toJSON().toString());
			}
			
			SQL_INSERT_COIN.setDouble(14,zCoin.getTokenAmount().getAsDouble());
			
			//Run the query
			SQL_INSERT_COIN.execute();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public long getMaxCoinBlock() {
		try {
			
			//Run the query
			ResultSet rs = SQL_LATEST_COIN.executeQuery();
			
			//Multiple results
			if(rs.next()) {
				long maxblock = rs.getLong("maxblock");
				return maxblock;
			}
		
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return -1;
	}
	
	public long getTotalCoins() {
		try {
			
			//Run the query
			ResultSet rs = SQL_TOTAL_COIN.executeQuery();
			
			//Multiple results
			if(rs.next()) {
				long count = rs.getLong("tot");
				return count;
			}
		
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return 0;
	}
	
	public synchronized JSONObject searchCoins(String zQuery,boolean zHideToken) {
		JSONObject error = new JSONObject();
		
		try {
		
			//Create the various tables..
			Statement stmt = mConnection.createStatement();
		
			JSONObject results = new JSONObject();
			results.put("sql", zQuery);
			error.put("sql", zQuery);
			
			//Execute the SQL..
			boolean res = stmt.execute(zQuery);
			
			if(res) {
				
				//Get the Results..
				ResultSet resset = stmt.getResultSet();
			
				//The data arrays
				JSONArray allrows      = new JSONArray();
				
				//Get the Headers..
				ResultSetMetaData rsmd = resset.getMetaData();
				int columnnum          = rsmd.getColumnCount();
				
				//Get the Results..
				int counter=0;
				while(resset.next()) {
					counter++;
					JSONObject row = new JSONObject();
					for(int i=1;i<=columnnum;i++) {
						String column = rsmd.getColumnName(i);
						Object obj    = resset.getObject(i);
						
						//Sometimes the tokens are large..
						if(zHideToken && column.equals("token")) {
							obj = new String("_hidden_");
						}
						
						//Make sure NOT NULL - or Omit.. 
						if(obj!=null) {

							//Treat some type special
							String type = rsmd.getColumnClassName(i);
							if(type.equals("java.sql.Clob")) {
								if(zHideToken && column.equals("token")) {
									row.put(column, obj.toString());
								}else {
									java.sql.Clob clob = (java.sql.Clob)obj;
		                        	String strvalue = clob.getSubString(1, (int) clob.length());
		                        	row.put(column, strvalue);
								}
								
							}else if(type.equals("java.lang.Double")) {
								
								//Format correctly.. 
								Double dd = (Double)obj;
								String val = String.format("%f", dd);
								row.put(column, val);
								
							}else {
								row.put(column, obj.toString());
							}
						}
					}
					allrows.add(row);
				}
				
				//There are results..
				results.put("status", true);
				results.put("results", true);
				results.put("count",counter);
				results.put("rows", allrows);
				
			}else {
				//There are results..
				results.put("status", true);
				results.put("results", false);
			}
			
			//Close
			stmt.close();
		
			return results;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
			error.put("status", false);
			error.put("results", false);
			error.put("error", true);
		}
		
		return error;
	}
	
	public synchronized boolean saveTxPoW(TxPoW zTxPoW) {
		try {
			
			//get the MiniData version..
			MiniData syncdata = MiniData.getMiniDataVersion(zTxPoW);
			
			//Get the Query ready
			SQL_INSERT_TXPOW.clearParameters();
		
			//Set main params
			SQL_INSERT_TXPOW.setString(1, zTxPoW.getTxPoWID());
			SQL_INSERT_TXPOW.setBytes(2, syncdata.getBytes());
			
			//Do it.
			SQL_INSERT_TXPOW.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized TxPoW getTxPoW(String zTxPoWID) {
		
		try {
			
			//Set search params
			SQL_GET_TXPOW.clearParameters();
			SQL_GET_TXPOW.setString(1, zTxPoWID);
			
			//Run the query
			ResultSet rs = SQL_GET_TXPOW.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the details..
				byte[] syncdata 	= rs.getBytes("txpowdata");
				
				//Create MiniData version
				MiniData minisync = new MiniData(syncdata);
				
				//Convert
				TxPoW sb = TxPoW.convertMiniDataVersion(minisync);
				
				return sb;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}

	public static void main(String[] zArgs) throws SQLException {
		
		//Load the required classes
		try {
			Class.forName("com.mysql.cj.jdbc.Driver");
		} catch (ClassNotFoundException e1) {
			e1.printStackTrace();
		}
		
		MySQLConnect mysql = new MySQLConnect("localhost:3306", "mydatabase", "myuser", "myuser");
		mysql.init();
				
		//Add some TxPoW..
		TxPoW txp = new TxPoW();
		txp.setBlockNumber(MiniNumber.ZERO);
		txp.setTimeMilli(MiniNumber.ZERO);
		txp.calculateTXPOWID();
	
		System.out.println("ID:"+txp.getTxPoWID());
		
		mysql.saveTxPoW(txp);
		
		//mysql.saveTxPoW(txp);
	
		//0xA04535D07E88F9AF7E9F8F706E4509526CB4E95203AB7730B7E4B7EF6B53D38B
		//0x90C64548848ED69AF2F50D73358549AA6FFB809A2C186B769340680ECE8248CA
		
		//Now load it..
		TxPoW tp = mysql.getTxPoW("0xA04535D07E88F9AF7E9F8F706E4509526CB4E95203AB7730B7E4B7EF6B53D38B");
		
		MinimaLogger.log(tp.toJSON().toString());
		
		
//		TxBlock txblk = new TxBlock(txp);
//		
//		txp = new TxPoW();
//		txp.setBlockNumber(MiniNumber.ONE);
//		txp.calculateTXPOWID();
//		txblk = new TxBlock(txp);
//		
//		mysql.saveBlock(txblk);
//		
//		//Now search for the top block..
//		long firstblock = mysql.loadFirstBlock();
//		long lastblock 	= mysql.loadLastBlock();
//		
//		System.out.println("FIRST : "+firstblock);
//		System.out.println("LAST  : "+lastblock);
//		
////		String txpid = "0x0003E914FBF1C04C9E1B52E37A171CA870E5310B33E50B9DFA9DF0C044A24150";
////		TxBlock block = mysql.loadBlockFromID(txpid);
//		
////		TxBlock block = mysql.loadBlockFromNum(3);
////		MinimaLogger.log(block.getTxPoW().toJSON().toString());
//		
//		ArrayList<TxBlock> blocks = mysql.loadBlockRange(MiniNumber.ZERO);
//		MinimaLogger.log("FOUND : "+blocks.size());
//		for(TxBlock block : blocks) {
//			MinimaLogger.log(block.getTxPoW().getBlockNumber().toString());
//		}
		
		mysql.shutdown();
	}
	
}
