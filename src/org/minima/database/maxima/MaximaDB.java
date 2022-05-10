package org.minima.database.maxima;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

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
	PreparedStatement SQL_SELECT_CONTACT 			= null;
	
	/**
	 * A Cached list of the Hosts
	 */
	boolean mCacheValid 				= false;
	ArrayList<MaximaHost> mCachedHosts 	= null;
	
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
							+ "  `name` varchar(255) NOT NULL,"
							+ "  `extradata` blob NOT NULL,"
							+ "  `publickey` blob NOT NULL,"
							+ "  `currenthost` varchar(255) NOT NULL,"
							+ "  `currentpublickey` blob NOT NULL,"
							+ "  `mycurrenthost` varchar(255) NOT NULL,"
							+ "  `mycurrentpublickey` blob NOT NULL"
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
			SQL_DELETE_OLD_HOSTS	= mSQLConnection.prepareStatement("DELETE FROM hosts WHERE lastseen < ?");

			//Load all the hosts
			getAllHosts();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}

	public synchronized boolean newHost(MaximaHost zHost) {
		try {
			
			//Cache no longer valid
			mCacheValid = false;
			
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
		
		//Is it cached
		if(mCacheValid) {
			
			//Cycle through our current list
			ArrayList<MaximaHost> allhosts = getAllHosts();
			for(MaximaHost host : allhosts) {
				if(host.getHost().equals(zHost)) {
					return host;
				}
			}
			
			return null;
		}
		
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
	
	public synchronized MaximaHost loadHostFromPublicKey(String zPublicKey) {
		
		//Cycle through our current list
		ArrayList<MaximaHost> allhosts = getAllHosts();
		for(MaximaHost host : allhosts) {
			if(host.getPublicKey().to0xString().equals(zPublicKey)) {
				return host;
			}
		}
		
		return null;
	}
	
	public synchronized ArrayList<MaximaHost> getAllHosts() {
		
		//Is it cached
		if(mCacheValid) {
			return mCachedHosts;
		}
		
		//Get the current list
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
		
		//Now the cache is valid
		mCachedHosts 	= hosts;
		mCacheValid 	= true;
		
		return hosts;
	}
	
	public synchronized boolean updateHost(MaximaHost zMXHost) {
		
		try {
			
			mCacheValid 	= false;
			
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
