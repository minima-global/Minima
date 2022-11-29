package org.minima.database.archive;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.database.cascade.Cascade;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class MySQLConnect {

	public static final int MAX_SYNCBLOCKS = 1000; 
	
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
	
	PreparedStatement SAVE_CASCADE				= null;
	PreparedStatement LOAD_CASCADE				= null;
	
	public MySQLConnect(String zHost, String zDatabase, String zUsername, String zPassword) {
		mMySQLHost 	= zHost;
		mDatabase	= zDatabase;
		mUsername	= zUsername;
		mPassword	= zPassword;
	}
	
	public void init() throws SQLException {
		//MYSQL JDBC connection
		String mysqldb = "jdbc:mysql://"+mMySQLHost+"/"+mDatabase+"?autoReconnect=true";
				
		mConnection = DriverManager.getConnection(mysqldb,mUsername,mPassword);
	
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
		
		//All done..
		stmt.close();
		
		//Create some prepared statements..
		String insert 			= "INSERT IGNORE INTO syncblock ( txpowid, block, timemilli, syncdata ) VALUES ( ?, ? ,? ,? )";
		SQL_INSERT_SYNCBLOCK 	= mConnection.prepareStatement(insert);
		SQL_FIND_SYNCBLOCK_ID 	= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
		SQL_FIND_SYNCBLOCK_NUM 	= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block=?");
		SQL_SELECT_RANGE		= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>=? ORDER BY block ASC LIMIT "+MAX_SYNCBLOCKS);
				
		SAVE_CASCADE = mConnection.prepareStatement("INSERT INTO cascadedata ( cascadetip, fulldata ) VALUES ( ?, ? )");
		LOAD_CASCADE = mConnection.prepareStatement("SELECT fulldata FROM cascadedata ORDER BY cascadetip ASC LIMIT 1");
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
		stmt.close();
	}
	
	public boolean saveCascade(Cascade zCascade) throws SQLException {
		
//			//Store as a file..
//			File root 			= MinimaDB.getDB().getBaseDBFolder();
//			File cascadefile 	= new File(root,CASCADE_FILE); 
//
//			//Does it exist..
//			if(cascadefile.exists()) {
//				cascadefile.delete();
//			}
//			
//			//Write the file out..
//			try {
//				MiniFile.writeObjectToFile(cascadefile, zCascade);
//			} catch (IOException e) {
//				throw new SQLException(e);
//			}
			
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
		
//		//Store as a file..
//		File root 			= MinimaDB.getDB().getBaseDBFolder();
//		File cascadefile 	= new File(root,CASCADE_FILE); 
//
//		if(cascadefile.exists()) {
//			
//			//Read it in..
//			byte[] data = null;
//			try {
//				data = MiniFile.readCompleteFile(cascadefile);
//			} catch (IOException e) {
//				throw new SQLException(e);
//			}
//			
//			//Convert
//			return Cascade.convertMiniDataVersion(new MiniData(data));
//		}
//		
//		return null;
		
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
			
//			MinimaLogger.log("MYSQL stored synvblock "+zBlock.getTxPoW().getBlockNumber());
			
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
	
	/**
	 * Non Synchronized version of LoadBlockRange
	 * @throws SQLException 
	 */
	public ArrayList<TxBlock> loadBlockRangeNoSync(MiniNumber zStartBlock) throws SQLException {
		
		ArrayList<TxBlock> blocks = new ArrayList<>();
		
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
		
		return blocks;
	}
	
	public static void main(String[] zArgs) throws SQLException {
		
		
		MySQLConnect mysql = new MySQLConnect("localhost:3306", "mydatabase", "myuser", "myuser");
		
		mysql.init();
		
//		String txpid = "0x0003E914FBF1C04C9E1B52E37A171CA870E5310B33E50B9DFA9DF0C044A24150";
//		TxBlock block = mysql.loadBlockFromID(txpid);
		
//		TxBlock block = mysql.loadBlockFromNum(3);
//		MinimaLogger.log(block.getTxPoW().toJSON().toString());
		
		
		ArrayList<TxBlock> blocks = mysql.loadBlockRange(MiniNumber.ZERO);
		MinimaLogger.log("FOUND : "+blocks.size());
		for(TxBlock block : blocks) {
			MinimaLogger.log(block.getTxPoW().getBlockNumber().toString());
		}
		
		
		mysql.shutdown();
	}
	
}
