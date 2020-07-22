package org.minima.system.network.commands;

import java.io.File;

import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.minidapps.minilib.JSMiniLibUtil;
import org.minima.utils.SQLHandler;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;

public class SQL implements Runnable {

	String mSQL;
	String mMiniDAPPID;
	String mFinalResult = "";
	
	//Call back with the response when finished in JS
	Function   mCallback;
	Context    mContext;
	Scriptable mScope;
	
	public SQL(String zSQL, String zMiniDAPPID) {
		this(zSQL, zMiniDAPPID,null,null,null);
	}
	
	public SQL(String zSQL, String zMiniDAPPID, Function zCallback, Context zContext, Scriptable zScope) {
		mSQL        = zSQL;
		mMiniDAPPID = zMiniDAPPID;
		mCallback   = zCallback;
		mContext    = zContext;
		mScope      = zScope;
	}
	
	public String getFinalResult() {
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
			File temp = BackupManager.getTempFolder();
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
		
		//Now send the result back vis the callback..
		if(mCallback != null) {
			//Create a native JSON
			Object json = JSMiniLibUtil.makeJSONObject(mFinalResult, mContext, mScope);
			
			//Make a function variable list
			Object functionArgs[] = { json };
		    
			//Call the function..
			mCallback.call(mContext, mScope, mScope, functionArgs);
		}
	}

}
