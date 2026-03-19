package org.minima.system.mds.sql;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Statement;

import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MiniDAPPDB extends SqlDB {

	public String mUID;
	
	public MiniDAPPDB(String zUID) {
		super();
		
		mUID = zUID;
	}
	
	@Override
	protected void createSQL() {}
	
	/**
	 * Only one thread can access the db at a time
	 */
	public synchronized JSONObject executeSQL(String zSQL, boolean zWriteMode) {
		
		JSONObject results = new JSONObject();
		results.put("sql", zSQL);
		
		//Check the SQL for banned words..
		if(!zWriteMode) {
			String upperSQL = zSQL.toUpperCase();
			for(int i=0;i<BANNED_COMMANDS.length;i++) {
				if(upperSQL.contains(BANNED_COMMANDS[i])) {
					
					int index = upperSQL.indexOf(BANNED_COMMANDS[i]); 
					
					//No good.. return false
					MinimaLogger.log("MiniDAPPSQL uid:"+mUID+" sql:"+zSQL+" error:BANNED_COMMAND "
							+BANNED_COMMANDS[i]+" @ "+index+" :.. "+zSQL.substring(index),false);
					
					results.put("status", false);
					results.put("count",0);
					results.put("rows", new JSONArray());
					results.put("results", false);
					results.put("error", "BANNED_COMMAND:"+BANNED_COMMANDS[i]);
					
					return results;
				}
			}
		}
		
		//Run the SQL
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
						String column = rsmd.getColumnLabel(i);
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
			MinimaLogger.log("MiniDAPPSQL uid:"+mUID+" sql:"+zSQL+" error:"+e.toString(),false);
			
			results.put("status", false);
			results.put("count",0);
			results.put("rows", new JSONArray());
			results.put("results", false);
			results.put("error", e.toString());
		}	
		
		return results;
	}

	//A list of banned commands..
	private String[] BANNED_COMMANDS = {"CSVREAD", "CSVWRITE", "FILE_READ", "FILE_WRITE",
									    "RUNSCRIPT", "SCRIPT ", "SCRIPT TO", "SHUTDOWN",
									    "LINK", "LINKED", "ALIAS ",
									    "EXEC", "SHELL", "CALL "};
	
	
}
