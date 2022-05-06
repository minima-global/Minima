package org.minima.database.maxima;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class MaximaDB extends SqlDB {

	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_MAXIMA_HOST 		= null;
	PreparedStatement SQL_UPDATE_MAXIMA_HOST 		= null;
	PreparedStatement SQL_SELECT_MAXIMA_HOST 		= null;
	PreparedStatement SQL_SELECT_ALL_HOSTS 			= null;
	PreparedStatement SQL_DELETE_OLD_HOSTS 			= null;
	
	PreparedStatement SQL_INSERT_MAXIMA_CONTACT 	= null;
	
	public MaximaDB() {
		super();
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();

			//Create hosts table
			String hosts = "CREATE TABLE IF NOT EXISTS `hosts` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `host` varchar(255) NOT NULL UNIQUE,"
							+ "  `publickey` blob NOT NULL,"
							+ "  `privatekey` blob NOT NULL,"
							+ "  `lastseen` bigint NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(hosts);

			//Create contacts table
			String contacts = "CREATE TABLE IF NOT EXISTS `contacts` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `name` varchar(255) NOT NULL UNIQUE,"
							+ "  `publickey` blob NOT NULL,"
							+ "  `currenthost` varchar(255) NOT NULL,"
							+ "  `currentpublickey` blob NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(contacts);
			
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			SQL_SELECT_MAXIMA_HOST	= mSQLConnection.prepareStatement("SELECT * FROM hosts WHERE host=?");
			SQL_SELECT_ALL_HOSTS	= mSQLConnection.prepareStatement("SELECT * FROM hosts");
			SQL_INSERT_MAXIMA_HOST	= mSQLConnection.prepareStatement("INSERT IGNORE INTO hosts ( host, publickey, privatekey, lastseen ) VALUES ( ?, ? ,? ,? )");
			SQL_UPDATE_MAXIMA_HOST	= mSQLConnection.prepareStatement("UPDATE hosts SET publickey=?, privatekey=?, lastseen=? WHERE host=?");

//			//Select 
//			SQL_FIND_SYNCBLOCK 		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
//			SQL_EXISTS_SYNCBLOCK	= mSQLConnection.prepareStatement("SELECT block FROM syncblock WHERE txpowid=?");
//			SQL_TOTAL_COUNT			= mSQLConnection.prepareStatement("SELECT COUNT(*) as tot FROM syncblock");
//			SQL_SELECT_RANGE		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<? ORDER BY block DESC");
//			SQL_DELETE_TXBLOCKS		= mSQLConnection.prepareStatement("DELETE FROM syncblock WHERE timemilli < ?");
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}

	public synchronized boolean newHost(MaximaHost zHost) {
		try {
			
			//Get the Query ready
			SQL_INSERT_MAXIMA_HOST.clearParameters();
		
			//Set main params
			SQL_INSERT_MAXIMA_HOST.setString(1, zHost.getHost());
			SQL_INSERT_MAXIMA_HOST.setBytes(2, zHost.getPublicKey().getBytes());
			SQL_INSERT_MAXIMA_HOST.setBytes(3, zHost.getPrivateKey().getBytes());
			SQL_INSERT_MAXIMA_HOST.setLong(4, zHost.getLastSeen());
			
			//Do it.
			SQL_INSERT_MAXIMA_HOST.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized MaximaHost loadHost(String zHost) {
		
		try {
			
			//Set search params
			SQL_SELECT_MAXIMA_HOST.clearParameters();
			SQL_SELECT_MAXIMA_HOST.setString(1, zHost);
			
			//Run the query
			ResultSet rs = SQL_SELECT_MAXIMA_HOST.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the details..
				MaximaHost mxhost = new MaximaHost(rs);
				
				return mxhost;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	public synchronized ArrayList<MaximaHost> loadAllHosts() {
		
		ArrayList<MaximaHost> hosts = new ArrayList<>();
		
		try {
			
			//Set Search params
			SQL_SELECT_ALL_HOSTS.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_ALL_HOSTS.executeQuery();
			
			//Multiple results
			while(rs.next()) {
				
				//Get the details..
				MaximaHost mxhost = new MaximaHost(rs);
				
				//Add to our list
				hosts.add(mxhost);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return hosts;
	}
	
	public synchronized boolean updateHost(MaximaHost zMXHost) {
		
		try {
			
			//Set search params
			SQL_UPDATE_MAXIMA_HOST.clearParameters();
			
			SQL_UPDATE_MAXIMA_HOST.setBytes(1, zMXHost.getPublicKey().getBytes());
			SQL_UPDATE_MAXIMA_HOST.setBytes(2, zMXHost.getPrivateKey().getBytes());
			SQL_UPDATE_MAXIMA_HOST.setLong(3, zMXHost.getLastSeen());
			
			SQL_UPDATE_MAXIMA_HOST.setString(4, zMXHost.getHost());
			
			//Run the query
			SQL_UPDATE_MAXIMA_HOST.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
}
