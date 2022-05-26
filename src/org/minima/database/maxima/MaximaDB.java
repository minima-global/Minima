package org.minima.database.maxima;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class MaximaDB extends SqlDB {

	/**
	 * 7 days then we can delete
	 */
	public static long MAX_HOST_INACTIVE = 1000 * 60 * 60 * 24 * 7;
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_MAXIMA_HOST 		= null;
	PreparedStatement SQL_UPDATE_MAXIMA_HOST 		= null;
	PreparedStatement SQL_UPDATE_ALL_NOTCONECTED 	= null;
	PreparedStatement SQL_SELECT_ALL_HOSTS 			= null;
	PreparedStatement SQL_DELETE_HOST 				= null;
	PreparedStatement SQL_DELETE_OLD_HOSTS 			= null;
	
	PreparedStatement SQL_INSERT_MAXIMA_CONTACT 	= null;
	PreparedStatement SQL_SELECT_ALL_CONTACTS 		= null;
	PreparedStatement SQL_SELECT_CONTACT_PUBLICKEY 	= null;
	PreparedStatement SQL_SELECT_CONTACT_ID 		= null;
	PreparedStatement SQL_UPDATE_CONTACT 			= null;
	PreparedStatement SQL_DELETE_CONTACT 			= null;
	
	/**
	 * A Cached list
	 */
	boolean mHostCacheValid 				= false;
	ArrayList<MaximaHost> mCachedHosts 		 = null;
	
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
							+ "  `connected` int NOT NULL,"
							+ "  `lastseen` bigint NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(hosts);

			//Create contacts table
			String contacts = "CREATE TABLE IF NOT EXISTS `contacts` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `extradata` blob NOT NULL,"
							+ "  `publickey` varchar(512) NOT NULL UNIQUE,"
							+ "  `currentaddress` varchar(512) NOT NULL,"
							+ "  `myaddress` varchar(512) NOT NULL,"
							+ "  `lastseen` bigint NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(contacts);
			
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			SQL_SELECT_ALL_HOSTS	= mSQLConnection.prepareStatement("SELECT * FROM hosts");
			SQL_INSERT_MAXIMA_HOST	= mSQLConnection.prepareStatement("INSERT IGNORE INTO hosts ( host, publickey, privatekey, connected, lastseen ) VALUES ( ?, ? , ? ,? ,? )");
			SQL_UPDATE_MAXIMA_HOST	= mSQLConnection.prepareStatement("UPDATE hosts SET publickey=?, privatekey=?, connected=?, lastseen=? WHERE host=?");
			SQL_DELETE_HOST			= mSQLConnection.prepareStatement("DELETE FROM hosts WHERE host=?");
			SQL_DELETE_OLD_HOSTS	= mSQLConnection.prepareStatement("DELETE FROM hosts WHERE connected=0 AND lastseen < ?");
			SQL_UPDATE_ALL_NOTCONECTED = mSQLConnection.prepareStatement("UPDATE hosts SET connected=0");
			
			SQL_INSERT_MAXIMA_CONTACT 	= mSQLConnection.prepareStatement("INSERT IGNORE INTO contacts "
					+ "( extradata, publickey, currentaddress, myaddress, lastseen ) VALUES ( ?, ?, ?, ?, ? )");
			
			SQL_SELECT_ALL_CONTACTS		 = mSQLConnection.prepareStatement("SELECT * FROM contacts");
			SQL_SELECT_CONTACT_PUBLICKEY = mSQLConnection.prepareStatement("SELECT * FROM contacts WHERE publickey=?");
			SQL_SELECT_CONTACT_ID 		 = mSQLConnection.prepareStatement("SELECT * FROM contacts WHERE id=?");
			
			SQL_UPDATE_CONTACT			= mSQLConnection.prepareStatement("UPDATE contacts SET "
					+ "extradata=?, currentaddress=?, myaddress=?, lastseen=? WHERE publickey=?");
			
			SQL_DELETE_CONTACT			= mSQLConnection.prepareStatement("DELETE FROM contacts WHERE id=?");
			
			//All Host are not connected
			allHostNotConnected();
			
			//Load all the hosts
			getAllHosts();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}

	/**
	 * At startup none are connected..
	 */
	private synchronized void allHostNotConnected() {
		
		try {
			
			mHostCacheValid 	= false;
			
			//Set search params
			SQL_UPDATE_ALL_NOTCONECTED.clearParameters();
			
			//Run the query
			SQL_UPDATE_ALL_NOTCONECTED.execute();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	
	}
	
	public synchronized boolean newHost(MaximaHost zHost) {
		try {
			
			//Cache no longer valid
			mHostCacheValid = false;
			
			//Get the Query ready
			SQL_INSERT_MAXIMA_HOST.clearParameters();
		
			//Set main params
			SQL_INSERT_MAXIMA_HOST.setString(1, zHost.getHost());
			SQL_INSERT_MAXIMA_HOST.setBytes(2, zHost.getPublicKey().getBytes());
			SQL_INSERT_MAXIMA_HOST.setBytes(3, zHost.getPrivateKey().getBytes());
			SQL_INSERT_MAXIMA_HOST.setInt(4, 1);
			SQL_INSERT_MAXIMA_HOST.setLong(5, zHost.getLastSeen());
			
			//Do it.
			SQL_INSERT_MAXIMA_HOST.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized MaximaHost loadHost(String zHost) {
		
		//Cycle through our current list
		ArrayList<MaximaHost> allhosts = getAllHosts();
		for(MaximaHost host : allhosts) {
			if(host.getHost().equals(zHost)) {
				return host;
			}
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
		if(mHostCacheValid) {
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
		mHostCacheValid 	= true;
		
		return hosts;
	}
	
	public synchronized boolean updateHost(MaximaHost zMXHost) {
		
		try {
			
			mHostCacheValid 	= false;
			
			//Set search params
			SQL_UPDATE_MAXIMA_HOST.clearParameters();
			
			SQL_UPDATE_MAXIMA_HOST.setBytes(1, zMXHost.getPublicKey().getBytes());
			SQL_UPDATE_MAXIMA_HOST.setBytes(2, zMXHost.getPrivateKey().getBytes());
			SQL_UPDATE_MAXIMA_HOST.setInt(3, zMXHost.getConnected());
			SQL_UPDATE_MAXIMA_HOST.setLong(4, zMXHost.getLastSeen());
			SQL_UPDATE_MAXIMA_HOST.setString(5, zMXHost.getHost());
			
			//Run the query
			SQL_UPDATE_MAXIMA_HOST.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized boolean deleteHost(String zHost) {
		
		try {
			
			mHostCacheValid 	= false;
			
			//Set search params
			SQL_DELETE_HOST.clearParameters();
			
			SQL_DELETE_HOST.setString(1, zHost);
			
			//Run the query
			SQL_DELETE_HOST.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized boolean deleteOldHosts() {
		
		try {
			
			mHostCacheValid 	= false;
			
			//Set search params
			SQL_DELETE_OLD_HOSTS.clearParameters();
			
			//The latest a host can be updated..
			long minmilli = System.currentTimeMillis() - MAX_HOST_INACTIVE;
			
			SQL_DELETE_OLD_HOSTS.setLong(1, minmilli);
			
			//Run the query
			SQL_DELETE_OLD_HOSTS.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized boolean newContact(MaximaContact zContact) {
		try {
			
			//Get the Query ready
			SQL_INSERT_MAXIMA_CONTACT.clearParameters();
			
			//Extra data
			MiniData extradata = MaximaContact.convertJSONObjectToData(zContact.getExtraData());
			SQL_INSERT_MAXIMA_CONTACT.setBytes(1, extradata.getBytes());
			
			SQL_INSERT_MAXIMA_CONTACT.setString(2, zContact.getPublicKey());
			SQL_INSERT_MAXIMA_CONTACT.setString(3, zContact.getCurrentAddress());
			SQL_INSERT_MAXIMA_CONTACT.setString(4, zContact.getMyAddress());
			SQL_INSERT_MAXIMA_CONTACT.setLong(5, System.currentTimeMillis());
			
			//Do it.
			SQL_INSERT_MAXIMA_CONTACT.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized ArrayList<MaximaContact> getAllContacts() {
		
		//Get the current list
		ArrayList<MaximaContact> contacts = new ArrayList<>();
		
		try {
			
			//Set Search params
			SQL_SELECT_ALL_CONTACTS.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_SELECT_ALL_CONTACTS.executeQuery();
			
			//Multiple results
			while(rs.next()) {
				
				//Get the details..
				MaximaContact mxcontact = new MaximaContact(rs);
				
				//Add to our list
				contacts.add(mxcontact);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return contacts;
	}

	public synchronized MaximaContact loadContactFromPublicKey(String zPublicKey) {
		
		try {
			
			//Set search params
			SQL_SELECT_CONTACT_PUBLICKEY.clearParameters();
			SQL_SELECT_CONTACT_PUBLICKEY.setString(1, zPublicKey);
			
			//Run the query
			ResultSet rs = SQL_SELECT_CONTACT_PUBLICKEY.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the details..
				MaximaContact mxcontact = new MaximaContact(rs);
				
				return mxcontact;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	public synchronized MaximaContact loadContactFromID(int zID) {
		
		try {
			
			//Set search params
			SQL_SELECT_CONTACT_ID.clearParameters();
			SQL_SELECT_CONTACT_ID.setLong(1, zID);
			
			//Run the query
			ResultSet rs = SQL_SELECT_CONTACT_ID.executeQuery();
			
			//Is there a valid result.. ?
			if(rs.next()) {
				
				//Get the details..
				MaximaContact mxcontact = new MaximaContact(rs);
				
				return mxcontact;
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	public synchronized boolean updateContact(MaximaContact zContact) {
		
		try {
			
			//Set search params
			SQL_UPDATE_CONTACT.clearParameters();
			
			//Extra Data a little different
			MiniData extradata = MaximaContact.convertJSONObjectToData(zContact.getExtraData());
			SQL_UPDATE_CONTACT.setBytes(1, extradata.getBytes());
			
			SQL_UPDATE_CONTACT.setString(2, zContact.getCurrentAddress());
			SQL_UPDATE_CONTACT.setString(3, zContact.getMyAddress());
			SQL_UPDATE_CONTACT.setLong(4, zContact.getLastSeen());
			
			SQL_UPDATE_CONTACT.setString(5, zContact.getPublicKey());
			
			//Run the query
			SQL_UPDATE_CONTACT.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized boolean deleteContact(int zID) {
		
		try {
			
			//Set search params
			SQL_DELETE_CONTACT.clearParameters();
			
			SQL_DELETE_CONTACT.setLong(1, zID);
			
			//Run the query
			SQL_DELETE_CONTACT.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
}
