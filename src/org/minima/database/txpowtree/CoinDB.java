package org.minima.database.txpowtree;

import java.io.File;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class CoinDB extends SqlDB {

	private static CoinDB mCoinDB = null;
	public static void createCoinDB(File zCoinDB) {
		mCoinDB = new CoinDB(zCoinDB);
	}
	public static CoinDB getTxPoWTreeCoinDB() {
		return mCoinDB;
	}
	
	/**
	 * Prepared SQL statements
	 */
	PreparedStatement SQL_INSERT_COIN 				= null;
	PreparedStatement SQL_SELECT_ALLCOINS 			= null;
	PreparedStatement SQL_CLEAR_COINS 				= null;
	PreparedStatement SQL_SIZE 						= null;
	
	private CoinDB(File zFile){
		 
		try {
			//First delete the file!
			String path 	= zFile.getAbsolutePath();
			File oldfile 	= new File(path+".mv.db");
			oldfile.delete();
			
			//Now load..
			loadDB(zFile);
			
		 }catch(SQLException exc) {
			 MinimaLogger.log(exc);
		 }
	}
	
	@Override
	protected void createSQL() throws SQLException {
		
		//Create the various tables..
		Statement stmt = mSQLConnection.createStatement();
		
		//Create main table
		String create = "CREATE TABLE IF NOT EXISTS `coins` ("
						+ "  `id` bigint auto_increment,"
						+ "  `txpowtreeid` varchar(80) NOT NULL,"
						+ "  `blockheight` int NOT NULL,"
						+ "  `coindata` blob NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//All done..
		stmt.close();
	
		//Prepared Statements
		SQL_INSERT_COIN	 	= mSQLConnection.prepareStatement("INSERT INTO coins ( txpowtreeid, blockheight, coindata ) VALUES ( ?, ?, ? )");
		SQL_SELECT_ALLCOINS = mSQLConnection.prepareStatement("SELECT * FROM coins WHERE txpowtreeid=?");
		SQL_CLEAR_COINS 	= mSQLConnection.prepareStatement("DELETE FROM coins WHERE blockheight<?");
		SQL_SIZE 			= mSQLConnection.prepareStatement("SELECT Count(*) as tot FROM coins");
	}
	
	/**
	 * This cleans the JDBC Connection.. so it can start and shutdown quicker
	 */
	public synchronized void closeAndReopen() {
		
		try {
			
			//First close the Connection and save DB.. compact
			saveDB(true);
			
			//And now reopen the DB..
			mSQLConnection = null;
			
			//And now re-open..
			checkOpen(false);
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public synchronized boolean insertCoin(MiniData zTxPoWTreeID, Coin zCoin, MiniNumber zBlock) {
		try {
			
			//Make sure..
			if(checkOpen()) {
				MinimaLogger.log("insertCoin CoinDB reopen required");
			}
			
			//get the MiniData version..
			MiniData coindata = MiniData.getMiniDataVersion(zCoin);
			
			//Get the Query ready
			SQL_INSERT_COIN.clearParameters();
		
			//Set main params
			SQL_INSERT_COIN.setString(1, zTxPoWTreeID.to0xString());
			SQL_INSERT_COIN.setLong(2, zBlock.getAsLong());
			SQL_INSERT_COIN.setBytes(3, coindata.getBytes());
			
			//Do it.
			SQL_INSERT_COIN.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
		
	}
	
	public synchronized ArrayList<Coin> getAllCoins(MiniData zTxPoWTreeID) {
		ArrayList<Coin> coins = new ArrayList<>();
		
		try {
			
			//Make sure..
			if(checkOpen()) {
				MinimaLogger.log("getAllCoins CoinDB reopen required");
			}
			
			//Get the Query ready
			SQL_SELECT_ALLCOINS.clearParameters();
		
			//Set main params
			SQL_SELECT_ALLCOINS.setString(1, zTxPoWTreeID.to0xString());
			
			//Run the query
			ResultSet rs = SQL_SELECT_ALLCOINS.executeQuery();
			
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the blob of data
				byte[] coindata = rs.getBytes("coindata");
				
				//Create MiniData version
				MiniData minicoin = new MiniData(coindata);
				
				//Convert into a Coin..
				Coin cc = Coin.convertMiniDataVersion(minicoin);
				
				//Add to our list
				coins.add(cc);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return coins;
	}
	
	public synchronized void clearOldCoins(long zMinimumBlock) {
		
		try {
			
			//Make sure..
			if(checkOpen()) {
				MinimaLogger.log("clearOldCoins CoinDB reopen required");
			}
			
			//Get the Query ready
			SQL_CLEAR_COINS.clearParameters();
		
			//Set main params
			SQL_CLEAR_COINS.setLong(1, zMinimumBlock);
			
			//Do it.
			SQL_CLEAR_COINS.execute();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public synchronized int getSize() {
		try {
			//Make sure..
			if(checkOpen()) {
				MinimaLogger.log("getSize CoinDB reopen required");
			}
			
			//Get the query ready
			SQL_SIZE.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SIZE.executeQuery();
			
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
}
