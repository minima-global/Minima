package org.minima.utils;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;

import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class SQLHandler {

	/**
	 * Logging
	 */
	private boolean SQL_LOGGING = false;
	
	/**
	 * Are we using MySQL
	 */
	static boolean mMySQL 			= false;
	static String mMySQLHost 		= "";
	static String mMySQLUser 		= "";
	static String mMySQLPassword 	= "";
	
	/**
	 * Have we attempted to create this DB before
	 */
	private static ArrayList<String> MYSQL_CREATDATABASE = new ArrayList<String>();
	
	public static boolean isMySQLEnabled() {
		return mMySQL;
	}
	
	public static boolean setMySQLDetails(String zHost, String zUser, String zPassword) {
		try {
			//Load MySQL classes
			Class.forName("com.mysql.cj.jdbc.Driver");
		
			//Create a test connection
			Connection conn = DriverManager.getConnection("jdbc:mysql://"+zHost+"/",zUser,zPassword);
			
			//If all good..
			conn.close();
			
			//Store these details
			mMySQL 			= true;
			mMySQLHost 		= zHost;
			mMySQLUser 		= zUser;
			mMySQLPassword  = zPassword;
			
		}catch (Exception e) {
			MinimaLogger.log(e);
			mMySQL = false;
		}
		
		return mMySQL;
	}
	
	/**
	 * Hash table of SQL connections currently 
	 */
	private static Hashtable<String, Connection> SQL_POOLS = new Hashtable<>();
	private static synchronized Connection getConnection(String zDataBaseURL, String zUser, String zPassword) throws SQLException {
		//Check if we have a pool already..
		if(!SQL_POOLS.containsKey(zDataBaseURL)) {
			 //Create new connection..
			 Connection newconn = DriverManager.getConnection(zDataBaseURL, zUser, zPassword);
			 
			 //Add it to the hashtable..
			 SQL_POOLS.put(zDataBaseURL, newconn);
			
			 //Created a connection
			 MinimaLogger.log("Create SQL Connection to "+zDataBaseURL);
		}
		
		//Get the Connection..
		Connection conn = SQL_POOLS.get(zDataBaseURL);
		if(conn.isClosed()) {
			//restart this..
			conn = DriverManager.getConnection(zDataBaseURL, zUser, zPassword);
			 
			//Add it to the hash table..
			SQL_POOLS.put(zDataBaseURL, conn);
			
			//Created a connection
			MinimaLogger.log("Closed connection restarted to "+zDataBaseURL);
		}
		
		//Return the Connection..
		return conn;
	}
	
	public static synchronized void CloseSQL() {
		Enumeration<Connection> allconns = SQL_POOLS.elements();
		while(allconns.hasMoreElements()) {
			Connection conn = allconns.nextElement();
			try {
				if(!conn.isClosed()) {
					conn.close();
				}
			} catch (SQLException e) {
				MinimaLogger.log(e);
			}
		}	
		
		//Clear it now..
		SQL_POOLS.clear();
		MinimaLogger.log("All database connections closed");	
	}
	
	
	public static String getMiniDappMySQLName(String zMiniDAPPID) {
		return "minidapp"+zMiniDAPPID;
	}
	
	//Connection to the Database
	Connection mSQLConnection;
	
	//The Database..
	String mDataBase;
	
	/**
	 * Constructors for both types of DB
	 */
	public SQLHandler(String zMiniDAppID) throws SQLException, ClassNotFoundException {
		this(zMiniDAppID, false);
	}
	
	public SQLHandler(String zMiniDAppID,boolean zFullPath) throws SQLException, ClassNotFoundException {
		//Create the Database URL
		if(mMySQL) {
			//Use ther MySQL JDBC
			String url = "jdbc:mysql://"+mMySQLHost+"/";
			String db  = getMiniDappMySQLName(zMiniDAppID);
			
			//Need to create the DB if not exists..
			if(!MYSQL_CREATDATABASE.contains(db)) {
				MYSQL_CREATDATABASE.add(db);
				
				Connection conn = DriverManager.getConnection(url,mMySQLUser,mMySQLPassword);
	
				//Get a statement
				Statement stmt = conn.createStatement();
				
				//Run some SQL..
				String query = "CREATE DATABASE IF NOT EXISTS "+db;
				stmt.execute(query);
				
				//Close connection
				conn.close();
			}
			
			//Now create the full Database url
			mDataBase = url+db;
			
			//Get the connection
			mSQLConnection = getConnection(mDataBase,mMySQLUser,mMySQLPassword);
			
		}else {
			if(!zFullPath) {
				//Calculate the Database file..
				BackupManager backup  = Main.getMainHandler().getBackupManager();
				File minidappdatabase = new File(backup.getMiniDAPPFolder(zMiniDAppID),"_sqldb");
				String path = minidappdatabase.getAbsolutePath();
				
				//Use H2 JDBC
				mDataBase = "jdbc:h2:"+path;
			}else {
				//Use H2 JDBC
				mDataBase = "jdbc:h2:"+zMiniDAppID;
			}
			
			//Get the connection
			mSQLConnection = getConnection(mDataBase,"SA","");
		}
	}
	
	public String getDataBaseURL() {
		return mDataBase;
	}
	
	public void close() throws SQLException {
		//Leave OPEN!
		close(false);
	}
	
	public void close(boolean zHard) throws SQLException {
		if(zHard) {
			mSQLConnection.close();
		}
	}
	
	public JSONArray executeMultiSQL(String zSQL) {
		JSONArray totalres = new JSONArray();
	
		StringTokenizer strtok = new StringTokenizer(zSQL, ";");
		while(strtok.hasMoreElements()) {
			String sql = strtok.nextToken().trim();
			
			if(!sql.equals("")) {
				//Run it..
				JSONObject res = executeSQL(sql);
				totalres.add(res);
				
				//Break on error
				if(res.get("status") == Boolean.FALSE) {
					break;
				}
			}
		}
		
		return totalres;
	}
	
	public JSONObject executeSQL(String zSQL) {
		JSONObject results = new JSONObject();
		results.put("sql", zSQL);
		
		try {
			//Create a statement to interact with te DB
			Statement stmt   = mSQLConnection.createStatement();

			if( zSQL.trim().toLowerCase().startsWith("update ") ||
				zSQL.trim().toLowerCase().startsWith("insert ") ||	
				zSQL.trim().toLowerCase().startsWith("delete ")) {
				int upd = stmt.executeUpdate(zSQL);
				results.put("status", true);
				results.put("results", false);
				results.put("update", upd);
				return results;
			}
			
			//Execute the SQL..
			boolean res = stmt.execute(zSQL);
				
			//Get the Results..
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
						//ALWAYS UPPERCASE
						String column = rsmd.getColumnName(i).toUpperCase();
						Object obj    = resset.getObject(i);
						row.put(column, obj.toString());					
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
			
			//Do we log..
			if(SQL_LOGGING) {
				MinimaLogger.log("SQL LOGGING : "+results.toString());
			}
			
		}catch(SQLException exc) {
			MinimaLogger.log("SQL ERROR @ "+mDataBase+" : "+zSQL);
			MinimaLogger.log(exc);
			
			results.put("status", false);
			results.put("message", exc.toString());
		}
						
		return results;
	}
	
	public static void main(String[] zArgs) {
		
		try {
//			SQLHandler handle = new SQLHandler("~/tester/temp/_tempdb",true);
			
			
//			for(int i=0;i<3;i++) {
			
				SQLHandler handle = new SQLHandler("110022");
				
				String sql = "CREATE TABLE IF NOT EXISTS preimage ( image VARCHAR(160) NOT NULL, hash VARCHAR(160) NOT NULL )";
				JSONObject results = handle.executeSQL(sql);
				System.out.println(sql);
				System.out.println(MiniFormat.JSONPretty(results.toString()));
				sql =     "INSERT INTO preimage (image, hash) VALUES ('xxx','hashxxx');"
						+ "SELECT * FROM preimage WHERE HASH='hashxxx';";
				JSONArray resultsarray = handle.executeMultiSQL(sql);
				System.out.println(MiniFormat.JSONPretty(resultsarray.toString()));
				
				//Close the connection
				handle.close(true);
//			}
			
			//Now delete
//				SQLHandler handle = new SQLHandler("110022");
//				
//				//Create the DROP SQL
//				String drop = "DROP DATABASE "+db;
				
				
			
//			//Create a Table..
//			String sql = "CREATE TABLE IF NOT EXISTS users ( "
//					+ "UID INTEGER IDENTITY PRIMARY KEY,"
//                    + "NAME VARCHAR(45) NOT NULL,"
//                    + "AGE DECIMAL(18,8) NOT NULL,"
//                    + "DOB DATETIME NOT NULL,"
//                    + "DESC LONGVARCHAR NULL,"
//                    + "EMAIL VARCHAR(45) NOT NULL)";
//			
//			JSONObject results = handle.executeSQL(sql);
//			System.out.println(sql);
//			System.out.println(MiniFormat.JSONPretty(results.toString()));
//			
//			//Now insert some stuff..
//			Random rr = new Random();
//			String insert = "INSERT INTO USERS (NAME,AGE,DOB,DESC,EMAIL) values "
//					+ "('paddy_"+rr.nextInt()+"', 22.345 , now(),'THIS IS A LONG STRING!','paddy@popo.com')";
//			JSONObject insertresults = handle.executeSQL(insert);
//			System.out.println(MiniFormat.JSONPretty(insertresults.toString()));
//			
//			//Now select some stuff..
//			String select = "SELECT * FROM USERS";
//			JSONObject selectresults = handle.executeSQL(select);
//			System.out.println(MiniFormat.JSONPretty(selectresults.toString()));
		
//			select = "DELETE FROM USERS";
//			selectresults = handle.executeSQL(select);
//			System.out.println(MiniFormat.JSONPretty(selectresults.toString()));
		
			
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
	}
	
}
