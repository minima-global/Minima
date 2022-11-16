package org.minima.system.mds.sql;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
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
	
	/**
	 * Only one thread can access the db at a atime
	 */
	public synchronized JSONObject executeSQL(String zSQL) {
		
		JSONObject results = new JSONObject();
		results.put("sql", zSQL);
		
		try {
			
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
			MinimaLogger.log("MiniDAPPSQL : "+e.toString());
			
			results.put("status", false);
			results.put("count",0);
			results.put("rows", new JSONArray());
			results.put("results", false);
			results.put("error", e.toString());
		}	
		
		return results;
	}

}
