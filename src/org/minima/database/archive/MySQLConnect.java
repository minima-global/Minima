package org.minima.database.archive;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class MySQLConnect {

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
	
	
	public MySQLConnect(String zHost, String zDatabase, String zUsername, String zPassword) {
		mMySQLHost 	= zHost;
		mDatabase	= zDatabase;
		mUsername	= zUsername;
		mPassword	= zPassword;
	}
	
	public void init() throws SQLException {
		//MYSQL JDBC connection
		String mysqldb = "jdbc:mysql://"+mMySQLHost+"/"+mDatabase;
				
		mConnection = DriverManager.getConnection(mysqldb,mUsername,mPassword);
	
		Statement stmt = mConnection.createStatement();
		
		//Create a new DB
		String create = "CREATE TABLE IF NOT EXISTS `syncblock` ("
						+ "  `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,"
						+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
						+ "  `block` bigint NOT NULL UNIQUE,"
						+ "  `timemilli` bigint NOT NULL,"
						+ "  `syncdata` blob NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//Create some prepared statements..
		String insert 			= "INSERT IGNORE INTO syncblock ( txpowid, block, timemilli, syncdata ) VALUES ( ?, ? ,? ,? )";
		SQL_INSERT_SYNCBLOCK 	= mConnection.prepareStatement(insert);
		SQL_FIND_SYNCBLOCK_ID 	= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
		SQL_FIND_SYNCBLOCK_NUM 	= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block=?");
		SQL_SELECT_RANGE		= mConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<? ORDER BY block DESC");
	}
	
	public void shutdown() {
		try {
			mConnection.close();
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public boolean saveBlock(TxBlock zBlock) {
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
			
			MinimaLogger.log("MYSQL stored synvblock "+zBlock.getTxPoW().getBlockNumber());
			
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

	public synchronized ArrayList<TxBlock> loadBlockRange(MiniNumber zStartBlock, MiniNumber zEndBlock) {
		
		ArrayList<TxBlock> blocks = new ArrayList<>();
		
		try {
			
			//Set Search params
			SQL_SELECT_RANGE.clearParameters();
			SQL_SELECT_RANGE.setLong(1,zStartBlock.getAsLong());
			SQL_SELECT_RANGE.setLong(2,zEndBlock.getAsLong());
			
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
	
	public static void main(String[] zArgs) throws SQLException {
		
		
		MySQLConnect mysql = new MySQLConnect("localhost:3306", "mydatabase", "myuser", "myuser");
		
		mysql.init();
		
//		String txpid = "0x0003E914FBF1C04C9E1B52E37A171CA870E5310B33E50B9DFA9DF0C044A24150";
//		TxBlock block = mysql.loadBlockFromID(txpid);
		
//		TxBlock block = mysql.loadBlockFromNum(3);
//		MinimaLogger.log(block.getTxPoW().toJSON().toString());
		
		
		ArrayList<TxBlock> blocks = mysql.loadBlockRange(MiniNumber.ZERO, MiniNumber.EIGHT);
		MinimaLogger.log("FOUND : "+blocks.size());
		for(TxBlock block : blocks) {
			MinimaLogger.log(block.getTxPoW().getBlockNumber().toString());
		}
		
		
		mysql.shutdown();
	}
	
}
