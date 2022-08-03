package org.minima.database.minidapps;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class MDSDB extends SqlDB {
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_MINIDAPP 		= null;
	PreparedStatement SQL_DELETE_MINIDAPP 		= null;
	PreparedStatement SQL_LIST_MINIDAPPS 		= null;
	PreparedStatement SQL_GET_MINIDAPP 			= null;
	
	public MDSDB() {
		super();
	}
	
	@Override
	protected void createSQL() throws SQLException {
			
		//Create the various tables..
		Statement stmt = mSQLConnection.createStatement();
		
		//Create main table
		String create = "CREATE TABLE IF NOT EXISTS `minidapps` ("
						+ "  `uid` varchar(80) NOT NULL,"
						+ "  `confdata` blob NOT NULL"
						+ ")";
		
		//Run it..
		stmt.execute(create);
		
		//All done..
		stmt.close();
		
		//Create some prepared statements..
		String insert 			= "INSERT IGNORE INTO minidapps ( uid, confdata ) VALUES ( ?, ? )";
		SQL_INSERT_MINIDAPP 	= mSQLConnection.prepareStatement(insert);
		
		SQL_DELETE_MINIDAPP		= mSQLConnection.prepareStatement("DELETE FROM minidapps WHERE uid = ?");
		SQL_LIST_MINIDAPPS		= mSQLConnection.prepareStatement("SELECT * FROM minidapps");
		SQL_GET_MINIDAPP		= mSQLConnection.prepareStatement("SELECT * FROM minidapps WHERE uid = ?");
	}
	
	public synchronized boolean insertMiniDAPP(MiniDAPP zDapp) {
		try {
			
			//Get the Query ready
			SQL_INSERT_MINIDAPP.clearParameters();
		
			//Set main params
			SQL_INSERT_MINIDAPP.setString(1, zDapp.getUID());
			
			MiniData confdata = convertJSONObjectToData(zDapp.getConfData());
			SQL_INSERT_MINIDAPP.setBytes(2, confdata.getBytes());
			
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
			
			//Set the UID
			SQL_DELETE_MINIDAPP.setString(1, zUID);
			
			//Run the query
			SQL_DELETE_MINIDAPP.executeUpdate();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public synchronized ArrayList<MiniDAPP> getAllMiniDAPPs(){

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
	
	public synchronized MiniDAPP getMiniDAPP(String zUID){

		try {
			
			//Set Search params
			SQL_GET_MINIDAPP.clearParameters();
			
			//Set the UID
			SQL_GET_MINIDAPP.setString(1, zUID);
			
			//Run the query
			ResultSet rs = SQL_GET_MINIDAPP.executeQuery();
			
			//Multiple results
			if(rs.next()) {
			
				//Get the MiniDAPP
				return new MiniDAPP(rs);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
}
