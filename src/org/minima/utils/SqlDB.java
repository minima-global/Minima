package org.minima.utils;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
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
		
		//Create the connection
		mSQLConnection = DriverManager.getConnection(h2db, "SA", "");
		
		//Auto commit changes
		mSQLConnection.setAutoCommit(true);

		//Perform Create SQL
		createSQL();
	}
	
	public boolean checkOpen() throws SQLException {
		
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
			MinimaLogger.log("SQLDB RE-OPEN : "+mSQLDBNoMV.getName());
			
			//Clean memory
			System.gc();
			
			//Get the full db path
			String path = mSQLDBNoMV.getAbsolutePath();
					
			//The H2 JDBC URL
			String h2db = "jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE";
			
			//Create the connection
			mSQLConnection = DriverManager.getConnection(h2db, "SA", "");
			
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
		
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void saveDB() {
		try {
		
			//Are we already closed..
			if(mSQLConnection.isClosed()) {
				return;
			}
			
			//One last statement
			Statement stmt = mSQLConnection.createStatement();
		
			//Shut down.. this saves and closes all the data
//			stmt.execute("SHUTDOWN COMPACT");
			stmt.execute("SHUTDOWN");

			//Close the connection
			mSQLConnection.close();
		
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void backupToFile(File zBackupFile) throws SQLException {
		
		//Delete file if exists..
		if(zBackupFile.exists()) {
			zBackupFile.delete();
		}
		
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//Create the backup Script
		String backup = String.format("SCRIPT TO '%s'", zBackupFile.getAbsolutePath());
		
		//Shut down.. this saves and closes all the data
		stmt.executeQuery(backup);
		
		//That's it..
		stmt.close();
	}
	
	public void restoreFromFile(File zRestoreFile) throws SQLException {
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//First wipe everything..
		stmt.execute("DROP ALL OBJECTS");
		
		//Create the backup Script
		String restore = String.format("RUNSCRIPT FROM '%s'", zRestoreFile.getAbsolutePath());
		
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
}
