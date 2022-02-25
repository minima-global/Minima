package org.minima.database.minidapps;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MiniDAPPDB extends SqlDB {

	public MiniDAPPDB() {
		super();
	}
	
	@Override
	protected void createSQL() {}
	
	public JSONObject executeSQL(String zSQL) {
		
		JSONObject results = new JSONObject();
		
		try {
			
			//Create the various tables..
			Statement stmt = mSQLCOnnection.createStatement();
		
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
						
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}	
		
		return results;
	}

}
