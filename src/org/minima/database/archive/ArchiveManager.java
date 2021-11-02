package org.minima.database.archive;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class ArchiveManager extends SqlDB {

	/**
	 * How long does data remains in the Archive DB
	 */
	public long MAX_SQL_MILLI = 1000 * 60 * 60 * 24 * GeneralParams.NUMBER_DAYS_ARCHIVE;
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_SYNCBLOCK 		= null;
	PreparedStatement SQL_FIND_SYNCBLOCK 		= null;
	PreparedStatement SQL_EXISTS_SYNCBLOCK 		= null;
	PreparedStatement SQL_SELECT_RANGE			= null;
	PreparedStatement SQL_TOTAL_COUNT 			= null;
	PreparedStatement SQL_DELETE_TXBLOCKS		= null;
	
	public ArchiveManager() {
		super();
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLCOnnection.createStatement();
			
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
			SQL_INSERT_SYNCBLOCK 	= mSQLCOnnection.prepareStatement(insert);
			
			//Select 
			SQL_FIND_SYNCBLOCK 		= mSQLCOnnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
			SQL_EXISTS_SYNCBLOCK	= mSQLCOnnection.prepareStatement("SELECT block FROM syncblock WHERE txpowid=?");
			SQL_TOTAL_COUNT			= mSQLCOnnection.prepareStatement("SELECT COUNT(*) as tot FROM syncblock");
			SQL_SELECT_RANGE		= mSQLCOnnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<? ORDER BY block DESC");
			SQL_DELETE_TXBLOCKS		= mSQLCOnnection.prepareStatement("DELETE FROM syncblock WHERE timemilli < ?");
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public synchronized int getSize() {
		try {
		
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
	
	public synchronized TxBlock loadBlock(String zTxPoWID) {
		
		try {
			
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
	
	public synchronized MiniNumber exists(String zTxPoWID) {
		
		try {
			
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
	
	public synchronized int cleanDB() {
		try {
			//Current MAX time..
			long maxtime = System.currentTimeMillis() - MAX_SQL_MILLI;
			
			//Set the parameters
			SQL_DELETE_TXBLOCKS.clearParameters();
			
			//Set the time milli
			SQL_DELETE_TXBLOCKS.setLong(1, maxtime);
			
			//Run the query
			return SQL_DELETE_TXBLOCKS.executeUpdate();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return 0;
	}
	
	public static void main(String[] zArgs) {
		
		File testdbfolder 	= new File(System.getProperty("user.home"),"testfolder");
		File testdb 		= new File(testdbfolder,"sqlsync");
		
		ArchiveManager arch = new ArchiveManager();
		arch.loadDB(testdb);
		
		//test insert..
		TxPoW txp = new TxPoW();
		txp.setBlockNumber(MiniNumber.ONE);
		txp.setTimeMilli();
		txp.calculateTXPOWID();
		txp.setSuperParent(0, new MiniData("0xFFEEFF"));
		
		//Create a SyncBlock
		TxBlock sb = new TxBlock(null,txp,new ArrayList<>());
		
		arch.saveBlock(sb);
		
		int rows = arch.getSize();
		
		System.out.println("DB Size : "+rows);
		
		String txpid = sb.getTxPoW().getTxPoWID();
		
		TxBlock lsb = arch.loadBlock(txpid);
		
		System.out.println("Sync Loaded : "+lsb.getTxPoW().toString());
		
		//Load a range..
		ArrayList<TxBlock> blocks = arch.loadBlockRange(MiniNumber.ZERO, MiniNumber.ONE);
		System.out.println("Sync Range : "+blocks.size());
		
		//Shut down
		arch.saveDB();
	}


	
}
