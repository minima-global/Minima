package org.minima.utils;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

public abstract class SqlDB {

	/**
	 * Main Connection to DB
	 */
	protected Connection mSQLConnection = null;
	
	/**
	 * The actual File used..
	 */
	protected File mSQLFile;
	
	public SqlDB() {}
	
	/**
	 * The actual database file on disk
	 */
	public File getSQLFile() {
		return mSQLFile;
	}
	
	/**
	 * Specify the location of the DB
	 */
	public void loadDB(File zFile) {
		
		//Make sure the parent files exist
		zFile.getParentFile().mkdirs();
		
		//Get the full db path
		String path = zFile.getAbsolutePath();
		
		//Store this
		mSQLFile = new File(path+".mv.db");
				
		try {
			//The H2 JDBC URL
			String h2db = "jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE";
			
			//Create the connection
			mSQLConnection = DriverManager.getConnection(h2db, "SA", "");
			
			//Save and compact the DB!
			Statement stmt = mSQLConnection.createStatement();
		
			//Shut down.. this saves and closes all the data
			stmt.execute("SHUTDOWN COMPACT");

			//Close the connection
			mSQLConnection.close();
			
			//Now open a NEW Connection..
			mSQLConnection = DriverManager.getConnection(h2db, "SA", "");
			
			//Auto commit changes
			mSQLConnection.setAutoCommit(true);
	
			//Perform Create SQL
			createSQL();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void saveDB() {
		try {
		
			//One last statement
			Statement stmt = mSQLConnection.createStatement();
		
			//Shut down.. this saves and closes all the data
			stmt.execute("SHUTDOWN COMPACT");

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
	protected abstract void createSQL();
	
}
