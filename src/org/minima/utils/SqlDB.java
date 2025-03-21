package org.minima.utils;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public abstract class SqlDB {

	/**
	 * Main Connection to DB
	 */
	protected Connection mSQLConnection = null;
	
	/**
	 * The actual File used..
	 */
	protected File mSQLFile;
	protected File mSQLDBNoMV;
	
	/**
	 * Is it Encrypted
	 */
	private boolean mEncrypted 			= false;
	private String mEncryptedPassword 	= "";
	
	public SqlDB() {}
	
	/**
	 * The actual database file on disk
	 */
	public File getSQLFile() {
		return mSQLFile;
	}
	
	/**
	 * Specify the location of the DB
	 * @throws SQLException 
	 */
	public void loadDB(File zFile) throws SQLException {
		
		//Store this for open checks
		mSQLDBNoMV = zFile;
		
		//Make sure the parent files exist
		zFile.getParentFile().mkdirs();
		
		//Get the full db path
		String path = zFile.getAbsolutePath();
		
		//Store this
		mSQLFile = new File(path+".mv.db");
				
		//The H2 JDBC URL
		String h2db = "jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE";
		
		if(Main.STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("SQLDB Connect.. "+h2db);
		}
		
		//Create the connection
		mSQLConnection = DriverManager.getConnection(h2db, "SA", "");
		
		//Auto commit changes
		mSQLConnection.setAutoCommit(true);

		if(Main.STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("SQLDB Connected.. "+h2db);
		}
		
		//Perform Create SQL
		createSQL();
	}
	
	public void loadEncryptedSQLDB(File zFile, String zPassword) throws SQLException {
		
		mEncrypted 			= true;
		mEncryptedPassword 	= zPassword;
		
		//Store this for open checks
		mSQLDBNoMV = zFile;
		
		//Make sure the parent files exist
		zFile.getParentFile().mkdirs();
		
		//Get the full db path
		String path = zFile.getAbsolutePath();
		
		//Store this
		mSQLFile = new File(path+".mv.db");
				
		//The H2 JDBC URL
		String h2db = "jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE;CIPHER=AES";
		
		//Create the connection
		mSQLConnection = DriverManager.getConnection(h2db, "SA", zPassword+" userpasswd");
		
		//Auto commit changes
		mSQLConnection.setAutoCommit(true);

		//Perform Create SQL
		createSQL();
	}
	
	public boolean isOpen() throws SQLException {
		
		if(mSQLConnection == null) {
			return false;
		}
		
		if(mSQLConnection.isClosed()) {
			return false;
		}
		
		
		return true;
	}
	
	public boolean checkOpen() throws SQLException {
		return checkOpen(true);
	}
	
	public boolean checkOpen(boolean zLogs) throws SQLException {
		
		//Check not NULL
		boolean reopen = false;
		if(mSQLConnection==null) {
			reopen = true;
			
		}else if(mSQLConnection.isClosed()) {
			reopen = true;
		}
		
		//Do we need to restart..
		if(reopen) {
			
			//Notify User
			if(zLogs) {
				MinimaLogger.log("SqlDB requires re-open : "+mSQLDBNoMV.getName());
			}
			
			//Clean memory
			System.gc();
			
			//Get the full db path
			String path = mSQLDBNoMV.getAbsolutePath();
					
			if(mEncrypted) {
				
				//The H2 JDBC URL
				String h2db = "jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE;CIPHER=AES";
				
				//Create the connection
				mSQLConnection = DriverManager.getConnection(h2db, "SA", mEncryptedPassword+" userpasswd");
			
			}else {
				
				//The H2 JDBC URL
				String h2db = "jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE";
				
				//Create the connection
				mSQLConnection = DriverManager.getConnection(h2db, "SA", "");
			}
			
			//Auto commit changes
			mSQLConnection.setAutoCommit(true);

			//Perform Create SQL
			createSQL();
		}
		
		return reopen;
	}
	
	public void hardCloseDB() {
		try {
		
			//Check not NULL
			if(mSQLConnection==null) {
				return;
			}
			
			//Are we already closed..
			if(mSQLConnection.isClosed()) {
				return;
			}
			
			//Close the connection
			mSQLConnection.close();
		
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
	
	public void saveDB(boolean zCompact) {
		try {
		
			//Check..
			if(mSQLConnection == null) {
				MinimaLogger.log("Trying to saveDB on NULL SQLDB");
				return;
			}
			
			//Are we already closed..
			if(mSQLConnection.isClosed()) {
				return;
			}
			
			//One last statement
			Statement stmt = mSQLConnection.createStatement();
		
			//Shut down.. this saves and closes all the data
			if(zCompact) {
				stmt.execute("SHUTDOWN COMPACT");
			}else {
				stmt.execute("SHUTDOWN");
			}

			//Close the connection
			mSQLConnection.close();
		
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
	
	public void backupToFile(File zBackupFile) throws SQLException {
		backupToFile(zBackupFile, false);
	}
	
	public void backupToFile(File zBackupFile, boolean zGZIP) throws SQLException {
		
		//Delete file if exists..
		if(zBackupFile.exists()) {
			zBackupFile.delete();
		}
		
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//Create the backup Script
		String backup = null;
		if(zGZIP) {
			backup = String.format("SCRIPT TO '%s' COMPRESSION GZIP", zBackupFile.getAbsolutePath());
		}else {
			backup = String.format("SCRIPT TO '%s'", zBackupFile.getAbsolutePath());
		}
		
		//Shut down.. this saves and closes all the data
		stmt.executeQuery(backup);
		
		//That's it..
		stmt.close();
	}
	
	public void restoreFromFile(File zRestoreFile) throws SQLException {
		restoreFromFile(zRestoreFile, false);
	}
	
	public void restoreFromFile(File zRestoreFile, boolean zGZIP) throws SQLException {
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//First wipe everything..
		stmt.execute("DROP ALL OBJECTS");
		
		//Create the backup Script
		String restore = null;
		if(zGZIP) {
			restore = String.format("RUNSCRIPT FROM '%s' COMPRESSION GZIP", zRestoreFile.getAbsolutePath());
		}else {
			restore = String.format("RUNSCRIPT FROM '%s'", zRestoreFile.getAbsolutePath());
		}
		
		//Shut down.. this saves and closes all the data
		stmt.execute(restore);
		
		//That's it..
		stmt.close();
	}
	
	/**
	 * Perform the Create SQL
	 */
	protected abstract void createSQL() throws SQLException;
	
	/**
	 * Utility Functions
	 */
	public static JSONObject convertDataToJSONObject(MiniData zData) throws ParseException {
		
		//First convert the Data back into a String
		MiniString str = new MiniString(zData.getBytes());
		
		//And now convert that String into a JSONOBject
		JSONObject json = (JSONObject) new JSONParser().parse(str.toString());
		
		return json;
	}
	
	public static MiniData convertJSONObjectToData(JSONObject zJSON) {
		
		//First convert the Data back into a String
		MiniString str = new MiniString(zJSON.toString());
		
		//And now convert that String into a MiniData
		MiniData data = new MiniData(str.getData());
		
		return data;
	}
	
	/**
	 * Only one thread can access the db at a time
	 */
	public synchronized JSONObject executeGenericSQL(String zSQL) {
		
		JSONObject results = new JSONObject();
		results.put("sql", zSQL);
		
		try {
			
			//Check is OPEN
			checkOpen();
			
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();
		
			//Execute the SQL..
			boolean res = stmt.execute(zSQL);
			
			if(res) {
				
				//Get the Results..
				ResultSet resset = stmt.getResultSet();
			
				//The data arrays
				JSONArray allrows      = new JSONArray();
				
				//Get the Headers..
				ResultSetMetaData rsmd = resset.getMetaData();
				int columnnum          = rsmd.getColumnCount();
				
				//Get the Results..
				int counter=0;
				while(resset.next()) {
					counter++;
					JSONObject row = new JSONObject();
					for(int i=1;i<=columnnum;i++) {
						String column = rsmd.getColumnName(i);
						Object obj    = resset.getObject(i);
						
						//Make sure NOT NULL - or Omit.. 
						if(obj!=null) {
							//Treat some type special
							if(rsmd.getColumnClassName(i).equals("java.sql.Clob")) {
								java.sql.Clob clob = (java.sql.Clob)obj;
	                        	String strvalue = clob.getSubString(1, (int) clob.length());
	                        	row.put(column, strvalue);
							
							}else {
								row.put(column, obj.toString());
							}
						}
					}
					allrows.add(row);
				}
				
				//There are results..
				results.put("status", true);
				results.put("results", true);
				results.put("count",counter);
				results.put("rows", allrows);
				
			}else {
				//There are results..
				results.put("status", true);
				results.put("results", false);
			}
			
			//Close the statement
			stmt.close();
						
		} catch (Exception e) {
			MinimaLogger.log("ExecuteSQL sql:"+zSQL+" error:"+e.toString(),false);
			
			results.put("status", false);
			results.put("count",0);
			results.put("rows", new JSONArray());
			results.put("results", false);
			results.put("error", e.toString());
		}	
		
		return results;
	}
}
