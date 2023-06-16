package org.minima.database.archive;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
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
	
	PreparedStatement SQL_SELECT_LAST_BLOCK		= null;
	PreparedStatement SQL_SELECT_FIRST_BLOCK	= null;
	PreparedStatement SQL_SELECT_BLOCK			= null;
	
	PreparedStatement SQL_SELECT_SYNC_LIST		= null;
	
	PreparedStatement SAVE_CASCADE				= null;
	PreparedStatement LOAD_CASCADE				= null;
	
	public ArchiveManager() {
		super();
	}
	
	@Override
	protected void createSQL() throws SQLException {
		
		if(Main.STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("Create ArchiveDB..");
		}
		
		//Create the various tables..
		Statement stmt = mSQLConnection.createStatement();
		
		//Create main table
		String create = "CREATE TABLE IF NOT EXISTS `syncblock` ("
						+ "  `id` bigint auto_increment,"
						+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
						+ "  `block` bigint NOT NULL UNIQUE,"
						+ "  `timemilli` bigint NOT NULL,"
						+ "  `syncdata` blob NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//Create the cascade table
		String cascade = "CREATE TABLE IF NOT EXISTS `cascadedata` ("
						+ "		`id` int auto_increment,"
						+ "		`cascadetip` BIGINT NOT NULL,"
						+ "		`fulldata` blob NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(cascade);
		
		//Create some fast indexes..
		String index = "CREATE INDEX IF NOT EXISTS fastsearch ON syncblock ( txpowid, block )";
				
		//Run it..
		stmt.execute(index);
		
		//All done..
		stmt.close();
		
		if(Main.STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("Create ArchiveDB.. finish");
		}
		
		//Create some prepared statements..
		String insert 			= "INSERT IGNORE INTO syncblock ( txpowid, block, timemilli, syncdata ) VALUES ( ?, ? ,? ,? )";
		SQL_INSERT_SYNCBLOCK 	= mSQLConnection.prepareStatement(insert);
		
		//Select 
		SQL_FIND_SYNCBLOCK 		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
		SQL_EXISTS_SYNCBLOCK	= mSQLConnection.prepareStatement("SELECT block FROM syncblock WHERE txpowid=?");
		SQL_TOTAL_COUNT			= mSQLConnection.prepareStatement("SELECT COUNT(*) as tot FROM syncblock");

		SQL_SELECT_RANGE		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<?");
		
		SQL_DELETE_TXBLOCKS		= mSQLConnection.prepareStatement("DELETE FROM syncblock WHERE block < ?");
		
		SQL_SELECT_LAST			= mSQLConnection.prepareStatement("SELECT * FROM syncblock ORDER BY block ASC LIMIT 1");
		SQL_SELECT_FIRST		= mSQLConnection.prepareStatement("SELECT * FROM syncblock ORDER BY block DESC LIMIT 1");
		
		SQL_SELECT_LAST_BLOCK	= mSQLConnection.prepareStatement("SELECT block FROM syncblock ORDER BY block ASC LIMIT 1");
		SQL_SELECT_FIRST_BLOCK	= mSQLConnection.prepareStatement("SELECT block FROM syncblock ORDER BY block DESC LIMIT 1");
		SQL_SELECT_BLOCK 		= mSQLConnection.prepareStatement("SELECT * FROM syncblock WHERE block=?");
		
		SQL_SELECT_SYNC_LIST	= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block<? AND block>=?");
	
		SAVE_CASCADE 			= mSQLConnection.prepareStatement("INSERT INTO cascadedata ( cascadetip, fulldata ) VALUES ( ?, ? )");
		LOAD_CASCADE 			= mSQLConnection.prepareStatement("SELECT fulldata FROM cascadedata ORDER BY cascadetip ASC LIMIT 1");
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
	
	public void checkCascadeRequired(Cascade zCascade) throws SQLException {
		
		if(zCascade.getLength()>0) {
			
			//Do we actually have a cascade yet
			Cascade casc = loadCascade();
			
			//if not.. store our one..
			if(casc == null) {
				MinimaLogger.log("Saving Cascade in ARCHIVEDB.. tip : "+zCascade.getTip().getTxPoW().getBlockNumber());
				saveCascade(zCascade);
			}else {
				MinimaLogger.log("Cascade already in ARCHIVEDB.. tip : "+casc.getTip().getTxPoW().getBlockNumber());
			}
		}
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
	
	public synchronized boolean saveBlock(TxBlock zBlock) throws SQLException {
		
		//Try Twice.. incase db shuts during..
		try {
			_intSaveBlock(zBlock);
			
		}catch(Exception exc) {
			
			//Try again..
			_intSaveBlock(zBlock);
		}
		
		return true;
	}
	
	private synchronized boolean _intSaveBlock(TxBlock zBlock) throws SQLException {
		
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
	
//	public synchronized TxBlock loadFirstBlock() {
//		
//		try {
//			
//			//Make sure..
//			checkOpen();
//		
//			//Set search params
//			SQL_SELECT_FIRST.clearParameters();
//			
//			//Run the query
//			ResultSet rs = SQL_SELECT_FIRST.executeQuery();
//			
//			//Is there a valid result.. ?
//			if(rs.next()) {
//				
//				//Get the details..
//				byte[] syncdata 	= rs.getBytes("syncdata");
//				
//				//Create MiniData version
//				MiniData minisync = new MiniData(syncdata);
//				
//				//Convert
//				TxBlock sb = TxBlock.convertMiniDataVersion(minisync);
//				
//				return sb;
//			}
//			
//		} catch (SQLException e) {
//			MinimaLogger.log(e);
//		}
//		
//		return null;
//	}
//
//	public synchronized TxBlock loadLastBlock() {
//		
//		try {
//			
//			//Make sure..
//			checkOpen();
//		
//			//Set search params
//			SQL_SELECT_LAST.clearParameters();
//			
//			//Run the query
//			ResultSet rs = SQL_SELECT_LAST.executeQuery();
//			
//			//Is there a valid result.. ?
//			if(rs.next()) {
//				
//				//Get the details..
//				byte[] syncdata 	= rs.getBytes("syncdata");
//				
//				//Create MiniData version
//				MiniData minisync = new MiniData(syncdata);
//				
//				//Convert
//				TxBlock sb = TxBlock.convertMiniDataVersion(minisync);
//				
//				return sb;
//			}
//			
//		} catch (SQLException e) {
//			MinimaLogger.log(e);
//		}
//		
//		return null;
//	}
	
	public synchronized TxBlock loadFirstBlock() {
		
		try {
			
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_SELECT_FIRST_BLOCK.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_FIRST_BLOCK.executeQuery();
			
			//Is there a valid result.. ?
			long block 		= -1;
			if(rs.next()) {
				
				//Get the details..
				block = rs.getLong("block");
			}
			
			if(block!=-1) {
				
				//Now get that block..
				SQL_SELECT_BLOCK.clearParameters();
				SQL_SELECT_BLOCK.setLong(1, block);
				
				//Run the query
				rs = SQL_SELECT_BLOCK.executeQuery();
				
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
			SQL_SELECT_LAST_BLOCK.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_LAST_BLOCK.executeQuery();
			
			//Is there a valid result.. ?
			long block 		= -1;
			if(rs.next()) {
				
				//Get the details..
				block = rs.getLong("block");
			}
			
			if(block!=-1) {
				
				//Now get that block..
				SQL_SELECT_BLOCK.clearParameters();
				SQL_SELECT_BLOCK.setLong(1, block);
				
				//Run the query
				rs = SQL_SELECT_BLOCK.executeQuery();
				
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
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
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
		return loadBlockRange(zStartBlock, zEndBlock, true);
	}
	
	public synchronized ArrayList<TxBlock> loadBlockRange(MiniNumber zStartBlock, MiniNumber zEndBlock, boolean zDescending) {
		
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
		if(zDescending) {
			Collections.sort(blocks, new Comparator<TxBlock>() {
				@Override
				public int compare(TxBlock zBlk1, TxBlock zBlk2) {
					return zBlk2.getTxPoW().getBlockNumber().compareTo(zBlk1.getTxPoW().getBlockNumber());
				}
			});
		}else {
			Collections.sort(blocks, new Comparator<TxBlock>() {
				@Override
				public int compare(TxBlock zBlk1, TxBlock zBlk2) {
					return zBlk1.getTxPoW().getBlockNumber().compareTo(zBlk2.getTxPoW().getBlockNumber());
				}
			});
		}
		
		return blocks;
	}
	
	public int checkForCleanDB() {
		if(!GeneralParams.ARCHIVE) {
			return cleanDB();
		}
		
		return 0;
	}
	
	private synchronized int cleanDB() {
		
		try {
			//Make sure..
			checkOpen();
		
			//Set search params
			SQL_SELECT_FIRST_BLOCK.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_FIRST_BLOCK.executeQuery();
			
			//Is there a valid result.. ?
			long block 		= -1;
			if(rs.next()) {
				
				//Get the details..
				block = rs.getLong("block");
			}else {
				return 0;
			}
			
			//Last block to keep
			MiniNumber cutoff = new MiniNumber(block).sub(new MiniNumber(MAX_KEEP_BLOCKS));
			
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
		arch.saveDB(false);
	}
	
}
