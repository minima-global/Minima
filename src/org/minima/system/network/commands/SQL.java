package org.minima.system.network.commands;

import java.io.File;

import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SQLHandler;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class SQL implements Runnable {

	String mSQL;
	String mMiniDAPPID;
	String mFinalResult = "";
	
	public SQL(String zSQL, String zMiniDAPPID) {
		mSQL        = zSQL;
		mMiniDAPPID = zMiniDAPPID;
	}
	
	public String getFinalResult() {
		return mFinalResult;
	}
	
	@Override
	public void run() {
		//The SQL results
		JSONObject res = new JSONObject();
		
		//Where is the database..
		BackupManager backup  = Main.getMainHandler().getBackupManager();
		File minidappdatabase = new File(backup.getMiniDAPPFolder(mMiniDAPPID),"_sqldb");
		
		//Get the Function..
		res.put("db", minidappdatabase.getAbsolutePath());
		res.put("sql", mSQL);
		
	    //Now lets do some SQL
		try {
			//Start the SQL handler
			SQLHandler handler = new SQLHandler(minidappdatabase.getAbsolutePath());
				
			//Run the SQL..
			if(mSQL.indexOf(";")!=-1) {
				JSONArray resp  = handler.executeMultiSQL(mSQL);
				res.put("status", true);
				res.put("response", resp);
			}else {
				JSONObject resp = handler.executeSQL(mSQL);	
				res.put("status", true);
				res.put("response", resp);
			}
			
			//Close it..
			handler.close();
			
		}catch (Exception e) {
			res.put("status", false);
			res.put("message", e.toString());
		}
		
		//The response returned..
		mFinalResult = res.toString();
	}

}
