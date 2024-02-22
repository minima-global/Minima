package org.minima.database.txpowdb.sql;

import java.io.File;
import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class TxPoWSqlDB extends SqlDB {

	/**
	 * How long does data remain the SQL DB in milli seconds
	 */
	public static long MAX_SQL_MILLI = 1000 * 60 * 60 * 24 * GeneralParams.NUMBER_DAYS_SQLTXPOWDB;
	
	/**
	 * The default MAX Relevant TxPoW
	 */
	public static MiniNumber MAX_RELEVANT_TXPOW = MiniNumber.HUNDRED;
	
	/**
	 * Prepared SQL statements
	 */
	PreparedStatement SQL_INSERT_TXPOW 		= null;
	PreparedStatement SQL_SELECT_TXPOW 		= null;
	PreparedStatement SQL_SELECT_CHILDREN 	= null;
	PreparedStatement SQL_TOTAL_TXPOW 		= null;
	PreparedStatement SQL_DELETE_TXPOW 		= null;
	PreparedStatement SQL_EXISTS 			= null;
	
	PreparedStatement SQL_SELECT_RELEVANT 	= null;
	
	public TxPoWSqlDB() {
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
			String create = "CREATE TABLE IF NOT EXISTS `txpow` ("
							+ "  `id` bigint auto_increment,"
							+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
							+ "  `isblock` tinyint NOT NULL,"
							+ "  `istransaction` tinyint NOT NULL,"
							+ "  `parentid` varchar(80) NOT NULL,"
							+ "  `timemilli` bigint NOT NULL,"
							+ "  `txpowdata` blob NOT NULL,"
							+ "  `isrelevant` tinyint NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(create);
			
			//Create some fast indexes..
			String index = "CREATE INDEX IF NOT EXISTS fastsearch ON txpow ( txpowid, parentid )";
					
			//Run it..
			stmt.execute(index);
			
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			String insert 		= "INSERT IGNORE INTO txpow ( txpowid, isblock, istransaction, parentid, timemilli, txpowdata, isrelevant ) VALUES ( ?, ? ,? ,? ,? ,? ,? )";
			SQL_INSERT_TXPOW 	= mSQLConnection.prepareStatement(insert);
			
			//Select 
			SQL_SELECT_TXPOW 	= mSQLConnection.prepareStatement("SELECT txpowdata FROM txpow WHERE txpowid=?");
			SQL_SELECT_CHILDREN	= mSQLConnection.prepareStatement("SELECT txpowid FROM txpow WHERE isblock=1 AND parentid=?");
			SQL_TOTAL_TXPOW		= mSQLConnection.prepareStatement("SELECT COUNT(*) AS tot FROM txpow");
			SQL_DELETE_TXPOW	= mSQLConnection.prepareStatement("DELETE FROM txpow WHERE timemilli < ? AND isrelevant=0");
			SQL_EXISTS			= mSQLConnection.prepareStatement("SELECT txpowid FROM txpow WHERE txpowid=?");
		
			SQL_SELECT_RELEVANT = mSQLConnection.prepareStatement("SELECT * FROM txpow WHERE isrelevant=1 ORDER BY timemilli DESC LIMIT ? OFFSET ?");
	}
	
	public void wipeDB() throws SQLException {
		
		//Make sure..
		checkOpen();
		
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//First wipe everything..
		stmt.execute("DROP ALL OBJECTS");
		
		//Create main table
		String create = "CREATE TABLE IF NOT EXISTS `txpow` ("
						+ "  `id` bigint auto_increment,"
						+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
						+ "  `isblock` tinyint NOT NULL,"
						+ "  `istransaction` tinyint NOT NULL,"
						+ "  `parentid` varchar(80) NOT NULL,"
						+ "  `timemilli` bigint NOT NULL,"
						+ "  `txpowdata` blob NOT NULL,"
						+ "  `isrelevant` tinyint NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//That's it..
		stmt.close();
	}
	
	public synchronized boolean addTxPoW(TxPoW zTxPoW, boolean zIsRelevant) {
		try {
			
			//Make sure..
			checkOpen();
			
			//get the MiniData version..
			MiniData txdata = MiniData.getMiniDataVersion(zTxPoW);
			
			//Get the Query ready
			SQL_INSERT_TXPOW.clearParameters();
		
			//Set main params
			SQL_INSERT_TXPOW.setString(1, zTxPoW.getTxPoWID());
			SQL_INSERT_TXPOW.setInt(2, new MiniByte(zTxPoW.isBlock()).getValue());
			SQL_INSERT_TXPOW.setInt(3, new MiniByte(zTxPoW.isTransaction()).getValue());
			SQL_INSERT_TXPOW.setString(4, zTxPoW.getParentID().to0xString());
			SQL_INSERT_TXPOW.setLong(5, System.currentTimeMillis());
			
			//The actual bytes
			SQL_INSERT_TXPOW.setBytes(6, txdata.getBytes());
			
			//Is it relevant
			if(zIsRelevant) {
				SQL_INSERT_TXPOW.setInt(7, 1);
			}else {
				SQL_INSERT_TXPOW.setInt(7, 0);
			}
			
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
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_TXPOW.clearParameters();
			
			//Set the txpowid we are searching for
			SQL_SELECT_TXPOW.setString(1, zTxPoWID);
		
			//Run the query
			ResultSet rs = SQL_SELECT_TXPOW.executeQuery();
			
			//Is there a result..
			if(rs.next()) {
				
				//Get the blob of data
				byte[] txpdata 	= rs.getBytes("txpowdata");
				
				//Create MiniData version
				MiniData minitxp = new MiniData(txpdata);
				
				//Convert into a TxPoW..
				TxPoW txpow = TxPoW.convertMiniDataVersion(minitxp);
				
				return txpow;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	public synchronized ArrayList<String> getChildBlocks(String zParentTxPoWID) {
		ArrayList<String> txpows = new ArrayList<>();

		try {
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_CHILDREN.clearParameters();
			
			//Set the txpowid we are seraching for
			SQL_SELECT_CHILDREN.setString(1, zParentTxPoWID);
		
			//Run the query
			ResultSet rs = SQL_SELECT_CHILDREN.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the txpowid
				String txpowid = rs.getString("txpowid");
				
				//Add to our list
				txpows.add(txpowid);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return txpows;
	}
	
	public ArrayList<TxPoW> getAllRelevant(int zLimit) {
		return getAllRelevant(zLimit, 0);
	}
	
	public synchronized ArrayList<TxPoW> getAllRelevant(int zLimit, int zOffset) {
		ArrayList<TxPoW> txpows = new ArrayList<>();

		try {
			
			//Make sure..
			checkOpen();
			
			//Get the query ready
			SQL_SELECT_RELEVANT.clearParameters();
			
			//Set the Limit..
			SQL_SELECT_RELEVANT.setInt(1, zLimit);
			SQL_SELECT_RELEVANT.setInt(2, zOffset);
			
			//Run the query
			ResultSet rs = SQL_SELECT_RELEVANT.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the blob of data
				byte[] txpdata 	= rs.getBytes("txpowdata");
				
				//Create MiniData version
				MiniData minitxp = new MiniData(txpdata);
				
				//Convert into a TxPoW..
				TxPoW txpow = TxPoW.convertMiniDataVersion(minitxp);
				
				//Add to our list
				txpows.add(txpow);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return txpows;
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

	public synchronized boolean exists(String zTxPoWID) {
		try {
			
			//Make sure..
			checkOpen();
			
			//Set the params..
			SQL_EXISTS.clearParameters();
			SQL_EXISTS.setString(1, zTxPoWID);
			
			//Run the query
			ResultSet rs = SQL_EXISTS.executeQuery();
			
			//Is there a row
			return rs.next();
			
		} catch (SQLException e) {
			//MinimaLogger.log(e);
		}
		
		return false;
	}
	
	/**
	 * Returns how many rows were deleted
	 */
	public synchronized int cleanDB() {
		return cleanDB(false);
	}
	
	public synchronized int cleanDB(boolean zHard) {
		try {
			
			//Make sure..
			checkOpen();
			
			//Current MAX time..
			long maxtime = System.currentTimeMillis() - MAX_SQL_MILLI;
			if(zHard) {
				maxtime = System.currentTimeMillis() + 100000;
			}
			
			//Set the parameters
			SQL_DELETE_TXPOW.clearParameters();
			
			//Set the time milli
			SQL_DELETE_TXPOW.setLong(1, maxtime);
			
			//Run the query
			return SQL_DELETE_TXPOW.executeUpdate();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return 0;
	}
	
	public static void main(String[] zArgs) throws IOException, SQLException {
		
		File testdb = new File(System.getProperty("user.home"),"testsql");
		
		TxPoWSqlDB db = new TxPoWSqlDB();
		
		db.loadDB(testdb);

		//test insert..
		TxPoW txp = new TxPoW();
		txp.setTimeMilli();
		txp.setSuperParent(0, new MiniData("0xFFEEFF"));
		txp.calculateTXPOWID();
		
		System.out.println("IN :"+txp.toJSON().toString());
		
		String id = txp.getTxPoWID();
		
		db.addTxPoW(txp, true);
		
//		Runnable rr = new Runnable() {
//			@Override
//			public void run() {
//				TxPoW txpread =  db.getTxPoW(id);
//				System.out.println("OUT:"+txpread.toJSON().toString());
//			}
//		};
//		Thread tt = new Thread(rr);
//		tt.start();
		
//		//Does it exis..
//		boolean exist = db.exists(id);
//		System.out.println("Excits TRUE : "+exist);
//		exist = db.exists("sdsdd");
//		System.out.println("Excits FALSE : "+exist);
//		
//		//get the children
//		ArrayList<String> children = db.getChildBlocks("0xFFEEFF");
//		System.out.println("Children : "+children);
//		
//		//Delete some rows..
//		db.MAX_SQL_MILLI = 20000;
//		long maxtime = System.currentTimeMillis() - 20000;
//		int deleted  = db.cleanDB();
//		System.out.println("Deleted : "+deleted);
//		
//		int total = db.getSize();
//		System.out.println("Total rows : "+total);
//		
//		System.out.println("File : "+db.getSQLFile());
//		System.out.println("File Size : "+db.getSQLFile().length());
		
		//Shutdown..
		db.saveDB(false);
	}
}
