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
	protected Connection mSQLCOnnection = null;
	
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
			//Create the connection - IGNORECASE=TRUE?
			mSQLCOnnection = DriverManager.getConnection("jdbc:h2:"+path+";MODE=MySQL", "SA", "");
//			mSQLCOnnection = DriverManager.getConnection("jdbc:h2:"+path+";MODE=MySQL;DB_CLOSE_ON_EXIT=FALSE", "SA", "");
			
			//Auto commit changes
			mSQLCOnnection.setAutoCommit(true);
	
			//Perform Create SQL
			createSQL();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void saveDB() {
		try {
		
			//One last statement
			Statement stmt = mSQLCOnnection.createStatement();
		
			//Shut down.. this saves and closes all the data
			stmt.execute("SHUTDOWN COMPACT");

			//Close the connection
			mSQLCOnnection.close();
		
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	/**
	 * Perform the Create SQL
	 */
	protected abstract void createSQL();
	
}
