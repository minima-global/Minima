package org.minima.system.network.minidapps.minilib;

import java.io.File;

import org.minima.system.backup.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.utils.SQLHandler;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class SQLCommand implements Runnable {

	String mSQL;
	String mMiniDAPPID;
	String mFinalResult = "";
	
	public SQLCommand(String zSQL, String zMiniDAPPID) {
		mSQL        = zSQL;
		mMiniDAPPID = zMiniDAPPID;
	}
	
	public String getFinalresult() {
		return mFinalResult;
	}
	
	@Override
	public void run() {
		//The SQL results
		JSONObject res = new JSONObject();
		
		//Where is the database..
		File minidappdatabase = null;
		BackupManager backup = InputHandler.getMainInputHandler().getMainHandler().getBackupManager();
		
		//Which Database.. could be running from a folder..
		if(mMiniDAPPID.equals("")) {
			//Get the database folder
			File temp = backup.getTempFolder();
			minidappdatabase = new File(temp,"_tempdb"+InputHandler.getMainInputHandler().RANDOM_VAL.to0xString());
			
		}else {
			//Get the database folder
			File minidapps   = backup.getMiniDAPPFolder();
			File dapp        = new File(minidapps,mMiniDAPPID);
			
			File dbdir       = new File(dapp,"sql");
			dbdir.mkdirs();
			
			minidappdatabase = new File(dbdir,"_sqldb");
		}
		
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
