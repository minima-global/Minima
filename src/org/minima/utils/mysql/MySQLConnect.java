package org.minima.utils.mysql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
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
					+ "  `token` text,"
					+ "  `tokenamount` double NOT NULL"
					+ ")";
			
			//Run it..
			stmt.execute(coins);
			
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
	
		SQL_INSERT_COIN = mConnection.prepareStatement("INSERT INTO coins(coinid,address,amount,amountdouble,tokenid,storestate,state,mmrentrynumber,spent,blockcreated,token,tokenamount) "
					+ "VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )");
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
	
	public synchronized void insertCoin(Coin zCoin) {
		
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
			
			SQL_INSERT_COIN.setString(7,zCoin.getStateAsJSON());
			SQL_INSERT_COIN.setLong(8,zCoin.getMMREntryNumber().getBigDecimal().longValue());
			
			if(zCoin.getSpent()) {
				SQL_INSERT_COIN.setInt(9,1);
			}else {
				SQL_INSERT_COIN.setInt(9,0);
			}
			
			SQL_INSERT_COIN.setLong(10,zCoin.getBlockCreated().getAsLong());
			
			if(zCoin.getToken() == null) {
				SQL_INSERT_COIN.setString(11,"");
			}else {
				SQL_INSERT_COIN.setString(11,zCoin.getToken().toJSON().toString());
			}
			
			SQL_INSERT_COIN.setDouble(12,zCoin.getTokenAmount().getAsDouble());
			
			//Run the query
			SQL_INSERT_COIN.execute();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public static void main(String[] zArgs) throws SQLException {
		
		//Load the required classes
		try {
			Class.forName("com.mysql.cj.jdbc.Driver");
		} catch (ClassNotFoundException e1) {
			e1.printStackTrace();
		}
		
		MySQLConnect mysql = new MySQLConnect("localhost:3306", "coinsdb", "myuser", "myuser");
		mysql.init();
		
		Coin cc = new Coin(new MiniData("0xFFEEDD"), new MiniNumber("100"), new MiniData("0x00"));
		mysql.insertCoin(cc);
		
		MinimaLogger.log("Coin inserted");
		
//		//Add some TxPoW..
//		TxPoW txp = new TxPoW();
//		txp.setBlockNumber(MiniNumber.ZERO);
//		txp.calculateTXPOWID();
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
