package org.minima.system.mds;

import java.io.File;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.mds.polling.PollHandler;
import org.minima.system.mds.polling.PollStack;
import org.minima.system.mds.sql.MiniDAPPDB;
import org.minima.system.mds.sql.SQLHandler;
import org.minima.system.network.rpc.HTTPServer;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MDSManager extends MessageProcessor {

	public static final String MDS_INIT 				= "MDS_INIT";
	public static final String MDS_POLLMESSAGE 			= "MDS_POLLMESSAGE";
	public static final String MDS_MINIDAPPS_CHANGED 	= "MDS_MINIDAPPS_CHANGED";
	
	HTTPServer mMDSServer;
	HTTPServer mPollServer;
	HTTPServer mSQLServer;
	
	File mMDSRootFile; 
	
	PollStack mPollStack;
	
	/**
	 * The SQL dbs..
	 */
	Hashtable<String, MiniDAPPDB> mSqlDB 	= new Hashtable<>();
	Object mSQLSyncObject 					= new Object();
	
	/**
	 * Valid MiniDAPPs
	 */
	ArrayList<String> mValid = new ArrayList<>();
	
	/**
	 * Main Constructor
	 */
	public MDSManager() {
		super("MDS");
		
		mPollStack = new PollStack();
		
		if(!GeneralParams.MDS_ENABLED) {
			MinimaLogger.log("MDS disabled");
			return;
		}
		
		PostMessage(MDS_INIT);
	}
	
	public void shutdown() {
		
		//Shut down the server
		if(GeneralParams.MDS_ENABLED) {
			mMDSServer.stop();
			mPollServer.stop();
			mSQLServer.stop();
		}
		
		//Save all the DBs
		Enumeration<MiniDAPPDB> dbs = mSqlDB.elements();
		while(dbs.hasMoreElements()) {
			MiniDAPPDB db = dbs.nextElement();
			db.saveDB();
		}
		
		stopMessageProcessor();
	}
	
	public File getRootMDSFolder() {
		return mMDSRootFile;
	}
	
	public File getWebFolder() {
		return new File(mMDSRootFile, "web");
	}
	
	public JSONObject runSQL(String zUID, String zSQL) {
		
		//Check / convert the UID..
		if(!mValid.contains(zUID)) {
			
			//Invalid..
			JSONObject fail = new JSONObject();
			fail.put("status", false);
			fail.put("error", "MiniDAPP not found : "+zUID);
			return fail;
		}
		
		String minidappid = zUID;
		
		//The final DB
		MiniDAPPDB db = null;
		
		//Synchronise access
		synchronized (mSQLSyncObject) {
			
			//Do we have it..
			db = mSqlDB.get(minidappid);
			
			//Does it exists yet
			if(db == null) {
			
				//Create the DB link
				db = new MiniDAPPDB();
				
				//The location
				File dbfolder1 = new File(getRootMDSFolder(),"data");
				File dbfolder2 = new File(dbfolder1,minidappid);
				File dbfolder3 = new File(dbfolder2,"sql");
				if(!dbfolder3.exists()) {
					dbfolder3.mkdirs();
				}
				
				//Now create the actual sql db
				db.loadDB(new File(dbfolder3,"sqldb"));
				
				MinimaLogger.log("SQL DB initialised for MiniDAPP : "+minidappid);
				
				//Add to the List
				mSqlDB.put(minidappid, db);
			}
		}
		
		//Now run the SQL
		JSONObject res = db.executeSQL(zSQL);
		
		return res;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Is it even enabled
		if(!GeneralParams.MDS_ENABLED) {
			return;
		}
		
		if(zMessage.getMessageType().equals(MDS_INIT)) {
			
			//What is the root folder
			mMDSRootFile = new File(GeneralParams.DATA_FOLDER,"mds");
			
			//Create a new Server
			mMDSServer = new HTTPServer(GeneralParams.MDS_PORT) {
				
				@Override
				public Runnable getSocketHandler(Socket zSocket) {
					return new MDSFileHandler( new File(mMDSRootFile,"web") , zSocket);
				}
			};
			
			//The Polling Server
			mPollServer = new HTTPServer(GeneralParams.POLL_PORT) {
				@Override
				public Runnable getSocketHandler(Socket zSocket) {
					return new PollHandler(mPollStack, zSocket);
				}
			};
			
			//The SQL Server
			mSQLServer = new HTTPServer(GeneralParams.SQL_PORT) {
				
				@Override
				public Runnable getSocketHandler(Socket zSocket) {
					return new SQLHandler(zSocket, MDSManager.this);
				}
			};
			
			//Scan for MiniDApps
			PostMessage(MDS_MINIDAPPS_CHANGED);
			
		}else if(zMessage.getMessageType().equals(MDS_POLLMESSAGE)) {
			
			// Add a message to the POll..
			JSONObject poll = (JSONObject) zMessage.getObject("poll");
			
			//Add to the Poll Stack
			mPollStack.addMessage(poll);
		
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_CHANGED)) {
			
			//Scan through and see what we have..
			ArrayList<MiniDAPP> dapps = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
			for(MiniDAPP dapp : dapps) {
			
				//Add to our valid list
				mValid.add(dapp.mUID);
				
				//Is there a service.js class
				
			}
			
			
		}
		
	}

}
