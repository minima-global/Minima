package org.minima.database.minidapps;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class MDSDB extends SqlDB {
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_MINIDAPP 		= null;
	PreparedStatement SQL_DELETE_MINIDAPP 		= null;
	PreparedStatement SQL_LIST_MINIDAPPS 		= null;
	
	public MDSDB() {
		super();
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();
			
			//Create main table
			String create = "CREATE TABLE IF NOT EXISTS `minidapps` ("
							+ "  `uid` varchar(80) NOT NULL,"
							+ "  `name` varchar(256) NOT NULL,"
							+ "  `icon` varchar(256) NOT NULL,"
							+ "  `version` varchar(256) NOT NULL,"
							+ "  `description` varchar(512) NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(create);
			
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			String insert 			= "INSERT IGNORE INTO minidapps ( uid, name, icon, version, description ) VALUES ( ?, ? ,? ,? ,? )";
			SQL_INSERT_MINIDAPP 	= mSQLConnection.prepareStatement(insert);
			
			SQL_DELETE_MINIDAPP		= mSQLConnection.prepareStatement("DELETE FROM minidapps WHERE uid = ?");
			SQL_LIST_MINIDAPPS		= mSQLConnection.prepareStatement("SELECT * FROM minidapps ORDER BY name ASC");
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public synchronized boolean insertMiniDAPP(MiniDAPP zDapp) {
		try {
			
			//Get the Query ready
			SQL_INSERT_MINIDAPP.clearParameters();
		
			//Set main params
			SQL_INSERT_MINIDAPP.setString(1, zDapp.mUID);
			SQL_INSERT_MINIDAPP.setString(2, zDapp.mName);
			SQL_INSERT_MINIDAPP.setString(3, zDapp.mIcon);
			SQL_INSERT_MINIDAPP.setString(4, zDapp.mVersion);
			SQL_INSERT_MINIDAPP.setString(5, zDapp.mDescription);
			
			//Do it.
			SQL_INSERT_MINIDAPP.execute();
			
			return true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return false;
	}
	
	public synchronized void deleteMiniDAPP(String zUID) {
		try {
			//Set the parameters
			SQL_DELETE_MINIDAPP.clearParameters();
			
			//Set the time milli
			SQL_DELETE_MINIDAPP.setString(1, zUID);
			
			//Run the query
			SQL_DELETE_MINIDAPP.executeUpdate();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public ArrayList<MiniDAPP> getAllMiniDAPPs(){

		ArrayList<MiniDAPP> dapps = new ArrayList<>();
		
		try {
			
			//Set Search params
			SQL_LIST_MINIDAPPS.clearParameters();
			
			//Run the query
			ResultSet rs = SQL_LIST_MINIDAPPS.executeQuery();
			
			//Multiple results
			while(rs.next()) {
			
				//Get the MiniDAPP
				MiniDAPP md = new MiniDAPP(rs);
				
				//Add to our list
				dapps.add(md);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return dapps;
	}
}