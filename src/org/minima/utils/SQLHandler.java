package org.minima.utils;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.StringTokenizer;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class SQLHandler {

	//Connection to the Database
	Connection mSQLConnection;
	
	public SQLHandler(String zDatabaseAbsolutePath) throws SQLException, ClassNotFoundException {
		//Start a database Connection..
		mSQLConnection = DriverManager.getConnection("jdbc:h2:"+zDatabaseAbsolutePath, "SA", "");
	}
	
	public void close() throws SQLException {
		mSQLConnection.close();
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
						String column = rsmd.getColumnName(i);
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
			
		}catch(SQLException exc) {
			results.put("status", false);
			results.put("message", exc.toString());
		}
						
		return results;
	}
	
	public static void main(String[] zArgs) {
		
		try {
			SQLHandler handle = new SQLHandler("/home/spartacusrex/.minima/temp/_tempdb");
		
			String sql = "CREATE TABLE IF NOT EXISTS preimage ( image VARCHAR(160) NOT NULL, hash VARCHAR(160) NOT NULL )";
			JSONObject results = handle.executeSQL(sql);
			System.out.println(sql);
			System.out.println(MiniFormat.JSONPretty(results.toString()));
			
//			sql = "INSERT INTO preimage (image, hash) VALUES ('xxx','hashxxx')";
//			results = handle.executeSQL(sql);
//			System.out.println(sql);
//			System.out.println(MiniFormat.JSONPretty(results.toString()));
			
			sql =     "INSERT INTO preimage (image, hash) VALUES ('xxx','hashxxx');"
					+ "SELECT * FROM preimage WHERE HASH='hashxxx';"
					+ "sdsdSELECT * FROM preimage WHERE HASH='hashxxx';"
					+ "SELECT * FROM preimage WHERE HASH='hashxxx';";
			JSONArray resultsarray = handle.executeMultiSQL(sql);
//			System.out.println(sql);
			System.out.println(MiniFormat.JSONPretty(resultsarray.toString()));
			
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
		
			//Close the connection
			handle.close();
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
	}
	
}
