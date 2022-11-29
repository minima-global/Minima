package org.minima.database.archive;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.cascade.Cascade;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;
import org.minima.utils.json.JSONObject;

public class ArchiveManager extends SqlDB {

	/**
	 * How long does data remains in the Archive DB (~200 blocks per day)
	 */
	public long MAX_KEEP_BLOCKS = 2000 * GeneralParams.NUMBER_DAYS_ARCHIVE;
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_SYNCBLOCK 		= null;
	PreparedStatement SQL_FIND_SYNCBLOCK 		= null;
	PreparedStatement SQL_EXISTS_SYNCBLOCK 		= null;
	PreparedStatement SQL_SELECT_RANGE			= null;
	PreparedStatement SQL_TOTAL_COUNT 			= null;
	PreparedStatement SQL_DELETE_TXBLOCKS		= null;
	
	PreparedStatement SQL_SELECT_LAST			= null;
	PreparedStatement SQL_SELECT_FIRST			= null;
	PreparedStatement SQL_SELECT_SYNC_LIST		= null;
	
	/**
	 * Is there a MySQL backup of ALL the blocks..
	 */
	boolean mStoreMySQL = false;
	MySQLConnect mMySQL;
	
	public ArchiveManager() {
		super();
	}
	
	public void setupMySQL(String zHost, String zDB, String zUser, String zPassword) throws SQLException {
		
		MinimaLogger.log("MySQL integration for Archive node activated..");
		
		//New MySQL
		mMySQL = new MySQLConnect(zHost, zDB, zUser, zPassword);
		
		//Initialise it..
		mMySQL.init();
		
		//We are storing in MySQL
		mStoreMySQL = true;
	}
	
	public boolean isStoreMySQL() {
		return mStoreMySQL;
	}
	
	public void checkCascadeRequired(Cascade zCascade) throws SQLException {
		
		if(isStoreMySQL() && zCascade.getLength()>0) {
			//Where does our archive start
			TxBlock gen = mMySQL.loadBlockFromNum(1);
			
			//Do we have it..
			if(gen!=null) {
				//we have it.. no cascade required..
				return;
			}
			
			//Do we actually have a cascade yet
			Cascade casc = mMySQL.loadCascade();
			
			//if not.. store our one..
			if(casc == null) {
				MinimaLogger.log("Saving Cascade in ARCHIVEDB.. tip : "+zCascade.getTip().getTxPoW().getBlockNumber());
				mMySQL.saveCascade(zCascade);
			}else {
				MinimaLogger.log("Cascade in ARCHIVEDB.. tip : "+casc.getTip().getTxPoW().getBlockNumber());
			}
		}
	}
	
	public MySQLConnect getMySQLCOnnect() {
		return mMySQL;
	}
	
	@Override
	public void saveDB() {
		super.saveDB();
			
		if(mStoreMySQL) {
			mMySQL.shutdown();
		}
	}
	
	@Override
	protected void createSQL() throws SQLException {
			
		//Create the various tables..
		Statement stmt = mSQLConnection.createStatement();
		
		//Create main table
		String create = "CREATE TABLE IF NOT EXISTS `syncblock` ("
						+ "  `id` IDENTITY PRIMARY KEY,"
						+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
						+ "  `block` bigint NOT NULL UNIQUE,"
						+ "  `timemilli` bigint NOT NULL,"
						+ "  `syncdata` blob NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//Create some fast indexes..
		String index = "CREATE INDEX IF NOT EXISTS fastsearch ON syncblock ( txpowid, block )";
				
		//Run it..
		stmt.execute(index);
		
		//All done..
		stmt.close();
		
		//Create some prepared statements..
		String insert 			= "INSERT IGNORE INTO syncblock ( txpowid, block, timemilli, syncdata ) VALUES ( ?, ? ,? ,? )";
		SQL_INSERT_SYNCBLOCK 	= mSQLConnection.prepareStatement(insert);
		
		//Select 
		SQL_FIND_SYNCBLOCK 		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
		SQL_EXISTS_SYNCBLOCK	= mSQLConnection.prepareStatement("SELECT block FROM syncblock WHERE txpowid=?");
		SQL_TOTAL_COUNT			= mSQLConnection.prepareStatement("SELECT COUNT(*) as tot FROM syncblock");

//		SQL_SELECT_RANGE		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<? ORDER BY block DESC");
		SQL_SELECT_RANGE		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<?");
		
		//SQL_DELETE_TXBLOCKS		= mSQLConnection.prepareStatement("DELETE FROM syncblock WHERE timemilli < ?");
		SQL_DELETE_TXBLOCKS		= mSQLConnection.prepareStatement("DELETE FROM syncblock WHERE block < ?");
		
		SQL_SELECT_LAST			= mSQLConnection.prepareStatement("SELECT * FROM syncblock ORDER BY block ASC LIMIT 1");
		SQL_SELECT_FIRST		= mSQLConnection.prepareStatement("SELECT * FROM syncblock ORDER BY block DESC LIMIT 1");
		
//		SQL_SELECT_SYNC_LIST	= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block<? ORDER BY block DESC LIMIT 100");
		SQL_SELECT_SYNC_LIST	= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block<? AND block>=?");
	}
	
	public synchronized int getSize() {
		try {
		
			//Make sure..
			checkOpen();
			
			//Run the query
			ResultSet rs = SQL_TOTAL_COUNT.executeQuery();
			
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
	
	public synchronized boolean saveBlock(TxBlock zBlock) throws SQLException {
		
		//Make sure..
		checkOpen();
	
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
	
		//Do we MySQL
		if(mStoreMySQL) {
			mMySQL.saveBlock(zBlock);
		}
		
		return true;		
	}
	
	public synchronized TxBlock loadBlock(String zTxPoWID) {
		
		try {
			
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_FIND_SYNCBLOCK.clearParameters();
			SQL_FIND_SYNCBLOCK.setString(1, zTxPoWID);
			
			//Run the query
			ResultSet rs = SQL_FIND_SYNCBLOCK.executeQuery();
			
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
	
	public synchronized TxBlock loadLastBlock() {
		
		try {
			
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_SELECT_LAST.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_LAST.executeQuery();
			
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
	
	public synchronized JSONObject loadLastBlockJSON() {
		
		JSONObject ret = new JSONObject();
		
		try {
			
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_SELECT_LAST.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_LAST.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				ret.put("txpowid", rs.getString("txpowid"));
				ret.put("block", rs.getBigDecimal("block").toString());
				ret.put("timemilli", rs.getBigDecimal("timemilli").toString());
				
				//Get the details..
				byte[] syncdata 	= rs.getBytes("syncdata");
				
				ret.put("bytes", syncdata.length);
				
				return ret;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return ret;
	}
	
	public synchronized ArrayList<TxBlock> loadSyncBlockRange(MiniNumber zStartBlock) {
		
		ArrayList<TxBlock> blocks = new ArrayList<>();
		
		try {
			
			//Make sure..
			checkOpen();
		
			//Set Search params
			SQL_SELECT_SYNC_LIST.clearParameters();
			SQL_SELECT_SYNC_LIST.setLong(1,zStartBlock.getAsLong());
			
			//The end block
			MiniNumber endblock = zStartBlock.sub(MiniNumber.TWOFIVESIX);
			if(endblock.isLessEqual(MiniNumber.ONE)) {
				endblock = MiniNumber.ONE;
			}
			SQL_SELECT_SYNC_LIST.setLong(2,endblock.getAsLong());
			
			//Run the query
			ResultSet rs = SQL_SELECT_SYNC_LIST.executeQuery();
			
			//Multiple results
			while(rs.next()) {
				
				//Are we shutting down..
				if(Main.getInstance().isShuttingDown()) {
					break;
				}
				
				//Get the details..
				byte[] syncdata = rs.getBytes("syncdata");
				
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
		
		//Now do the ordering.. MUCH FASTER than the SQL way..
		Collections.sort(blocks, new Comparator<TxBlock>() {
			@Override
			public int compare(TxBlock zBlk1, TxBlock zBlk2) {
				return zBlk2.getTxPoW().getBlockNumber().compareTo(zBlk1.getTxPoW().getBlockNumber());
			}
		});
		
		return blocks;
	}
	
	public synchronized MiniNumber exists(String zTxPoWID) {
		
		try {
			//Make sure..
			checkOpen();
			
			//Set search params
			SQL_EXISTS_SYNCBLOCK.clearParameters();
			SQL_EXISTS_SYNCBLOCK.setString(1, zTxPoWID);
			
			//Run the query
			ResultSet rs = SQL_EXISTS_SYNCBLOCK.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				//Get the block
				long block = rs.getLong("block");
				
				return new MiniNumber(block);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return MiniNumber.MINUSONE;
	}
	
	public synchronized ArrayList<TxBlock> loadBlockRange(MiniNumber zStartBlock, MiniNumber zEndBlock) {
		
		ArrayList<TxBlock> blocks = new ArrayList<>();
		
		try {
			
			//Make sure..
			checkOpen();
		
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
		
		//Now do the ordering.. MUCH FASTER than the SQL way..
		Collections.sort(blocks, new Comparator<TxBlock>() {
			@Override
			public int compare(TxBlock zBlk1, TxBlock zBlk2) {
				return zBlk2.getTxPoW().getBlockNumber().compareTo(zBlk1.getTxPoW().getBlockNumber());
			}
		});
		
		return blocks;
	}
	
	public synchronized TxBlock loadFirstBlock() {
		
		try {
			
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_SELECT_FIRST.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_FIRST.executeQuery();
			
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

	public synchronized int cleanDB() {
		try {
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_SELECT_FIRST.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_FIRST.executeQuery();
			
			//Is there a valid result.. ?
			TxBlock fb = null;
			if(rs.next()) {
				
				//Get the details..
				byte[] syncdata 	= rs.getBytes("syncdata");
				
				//Create MiniData version
				MiniData minisync = new MiniData(syncdata);
				
				//Convert
				fb = TxBlock.convertMiniDataVersion(minisync);
				
			}else {
				return 0;
			}
			
			//Last block to keep
			MiniNumber cutoff = fb.getTxPoW().getBlockNumber().sub(new MiniNumber(MAX_KEEP_BLOCKS));
			
			//Set the parameters
			SQL_DELETE_TXBLOCKS.clearParameters();
			
			//Set the time milli
			SQL_DELETE_TXBLOCKS.setLong(1, cutoff.getAsLong());
			
			//Run the query
			return SQL_DELETE_TXBLOCKS.executeUpdate();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return 0;
	}
	
	public void hackShut() throws SQLException {
		mSQLConnection.close();
	}
	
	public static void main(String[] zArgs) throws SQLException {
		
		File testdbfolder 	= new File(System.getProperty("user.home"),"testfolder");
		File testdb 		= new File(testdbfolder,"sqlsync");
		
		//Wipe the old..
		MiniFile.deleteFileOrFolder(testdbfolder.getAbsolutePath(), testdbfolder);
		
		ArchiveManager arch = new ArchiveManager();
		arch.loadDB(testdb);
		
		//test insert..
		TxPoW txp = new TxPoW();
		txp.setBlockNumber(MiniNumber.ONE);
		txp.setTimeMilli();
		txp.calculateTXPOWID();
		txp.setSuperParent(0, new MiniData("0xFFEEFF"));
		
		arch.hackShut();
		
		//Create a SyncBlock
		TxBlock sb = new TxBlock(txp);
		
		arch.saveBlock(sb);
		
		arch.hackShut();
		
		int rows = arch.getSize();
		
		System.out.println("DB Size : "+rows);
		
		arch.hackShut();
		
		String txpid = sb.getTxPoW().getTxPoWID();
		
		TxBlock lsb = arch.loadBlock(txpid);
		
		arch.hackShut();
		
		System.out.println("Sync Loaded : "+lsb.getTxPoW().toString());
		
		//Load a range..
		ArrayList<TxBlock> blocks = arch.loadBlockRange(MiniNumber.ZERO, MiniNumber.ONE);
		System.out.println("Sync Range : "+blocks.size());
		
		//Shut down
		arch.saveDB();
	}


	
}
