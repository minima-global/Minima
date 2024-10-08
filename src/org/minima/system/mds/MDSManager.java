package org.minima.system.mds;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.Socket;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.net.ssl.SSLSocket;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.system.mds.polling.PollStack;
import org.minima.system.mds.runnable.MDSJS;
import org.minima.system.mds.runnable.NullCallable;
import org.minima.system.mds.runnable.api.APICallback;
import org.minima.system.mds.runnable.shutter.SandboxContextFactory;
import org.minima.system.mds.sql.MiniDAPPDB;
import org.minima.system.network.rpc.HTTPSServer;
import org.minima.system.network.rpc.HTTPServer;
import org.minima.system.network.rpc.Server;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BaseConverter;
import org.minima.utils.JsonDB;
import org.minima.utils.Maths;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ZipExtractor;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.mozilla.javascript.ClassShutter;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class MDSManager extends MessageProcessor {

	public static final String MDS_INIT 					= "MDS_INIT";
	public static final String MDS_SHUTDOWN 				= "MDS_SHUTDOWN";
	public static final String MDS_POLLMESSAGE 				= "MDS_POLLMESSAGE";
	public static final String MDS_MINIDAPPS_RESETALL 		= "MDS_MINIDAPPS_RESETALL";
	public static final String MDS_MINIDAPPS_RESETSESSIONS 	= "MDS_MINIDAPPS_RESETSESSIONS";
	
	public static final String MDS_MINIDAPPS_INSTALLED 		= "MDS_MINIDAPPS_INSTALLED";
	public static final String MDS_MINIDAPPS_UNINSTALLED 	= "MDS_MINIDAPPS_UNINSTALLED";
	
	/**
	 * Timer Message sent every 10 seconds to MDS apps - frontend / backend
	 */
	public static final String MDS_TIMER_10SECONDS		= "MDS_TIMER_10SECONDS";
	public static final String MDS_TIMER_60SECONDS		= "MDS_TIMER_60SECONDS";
	public static final String MDS_TIMER_1HOUR			= "MDS_TIMER_1HOUR";
	
	/**
	 * Message sent to MiniDAPPs when shutdown occurs.
	 */
	public static final String MDS_SHUTDOWN_MSG			= "MDS_SHUTDOWN";
	
	//The Main File and Command server
	Server mMDSFileServer;
	
	File mMDSRootFile; 
	
	PollStack mPollStack;
		
	/**
	 * The SQL dbs..
	 */
	Hashtable<String, MiniDAPPDB> mSqlDB 	= new Hashtable<>();
	Object mSQLSyncObject 					= new Object();
	
	/**
	 * The KeyPair JSON
	 */
	Hashtable<String, JsonDB> mKeyPairDB 	= new Hashtable<>();
	Object mKeyPairSyncObject 				= new Object();
	
	/**
	 * Valid MiniDAPPs
	 */
	Hashtable<String, String> mSessionID 	= new Hashtable<>();
	
	/**
	 * The BASE MiniDAPP Password for the MiniHUB
	 */
	String mMiniHUBPassword = null;
	
	/**
	 * All the current Contexts
	 */
	ArrayList<MDSJS> mRunnables = new ArrayList();
	
	/**
	 * All the Pending Commands
	 */
	ArrayList<PendingCommand> mPending = new ArrayList<>();
	
	/**
	 * The Current Default MinHUB
	 */
	public String DEFAULT_MINIHUB = "0x00";
	
	/**
	 * Has MDS inited
	 */
	boolean mHasStarted 	= false;
	boolean mIsShuttingDown = false;
	
	/**
	 * List of all the API call objects
	 */
	ArrayList<APICallback> mAPICalls = new ArrayList<>();
	
	/**
	 * Public MDS uses this MiniDAPP Handle..
	 */
	MiniDAPP mPublicMiniDAPP;
	String mPublicMiniSessionID = "0xDEAD";
	String mPublicMiniUID 		= "0xFFDDEECCBBAA0099";
	
	/**
	 * Main Constructor
	 */
	public MDSManager() {
		super("MDS");
		
		mPollStack = new PollStack();
		
		//What is the root folder
		mMDSRootFile = new File(GeneralParams.DATA_FOLDER,"mds");
		
		//Is MDS even enabled
		if(!GeneralParams.MDS_ENABLED) {
			MinimaLogger.log("MDS disabled");
			return;
		}else {
			MinimaLogger.log("MDS enabled");
		}
		
		//Create the Public MiniDAPP
		JSONObject conf = new JSONObject();
		conf.put("name", "PublicMDS");
		conf.put("description", "Public MiniDAPPs used by the public");
		conf.put("version", "1.0");
		conf.put("permission", "read");
		mPublicMiniDAPP = new MiniDAPP(mPublicMiniUID, conf);
		
		//And Initialise the MDS properly
		PostMessage(MDS_INIT);
	}
	
	public boolean hasStarted() {
		return mHasStarted;
	}
	
	public boolean isShuttingDown() {
		return mIsShuttingDown;
	}
	
	public void shutdown() {
		//Is it even enabled
		if(!GeneralParams.MDS_ENABLED) {
			stopMessageProcessor();
			return;
		}
		
		//Send a SHUTDOWN message to all the MiniDAPP WEB sites..
		mPollStack.onlyShutDown();
		
		//This is for the JS Runnables
		Main.getInstance().PostNotifyEvent("MDS_SHUTDOWN", new JSONObject());
		
		//Wait 2 seconds for it to be processed..
		try {Thread.sleep(2000);} catch (InterruptedException e) {}
		
		//Now post a shutdown message - added to stack so will wait for POLL messages
		PostMessage(MDS_SHUTDOWN);
		
		//Waiting for shutdown..
		waitToShutDown();
		
		//No longer started
		mHasStarted = false;
	}
	
	public File getRootMDSFolder() {
		return mMDSRootFile;
	}
	
	public File getWebFolder() {
		return new File(mMDSRootFile, "web");
	}
	
	public File getDataFolder() {
		return new File(mMDSRootFile, "data");
	}
	
	public File getMiniDAPPWebFolder(String zUID) {
		return new File(getWebFolder(), zUID);
	}
	
	public File getMiniDAPPDataFolder(String zUID) {
		return new File(getDataFolder(), zUID);
	}
	
	public File getMiniDAPPFileFolder(String zUID) {
		return new File(getMiniDAPPDataFolder(zUID), "file");
	}
	
	public File getMiniDAPPSQLFolder(String zUID) {
		return new File(getMiniDAPPDataFolder(zUID), "sql");
	}
	
	public File getMiniDAPPKeyPairFolder(String zUID) {
		return new File(getMiniDAPPDataFolder(zUID), "keypair");
	}
	
	public File getMiniDAPPCopyDappFolder(String zUID) {
		return new File(getMiniDAPPFileFolder(zUID), "minidapp");
	}

	public String getMiniDAPPShareFileName(MiniDAPP zMiniDAPP) {
		String filename	= zMiniDAPP.getName().toLowerCase().replaceAll(" ", "");
		String fullname = filename+"-"+zMiniDAPP.getVersion()+".mds.zip";
		return fullname;
	}
	
	public File getMiniDAPPShareFile(String zUID) {
		MiniDAPP md 		= MinimaDB.getDB().getMDSDB().getMiniDAPP(zUID);
		return getMiniDAPPShareFile(md);
	}
	
	public File getMiniDAPPShareFile(MiniDAPP zMiniDAPP) {
		File copyfolder 	= getMiniDAPPCopyDappFolder(zMiniDAPP.getUID());
		String fullname 	= getMiniDAPPShareFileName(zMiniDAPP);
		File minisharefile 	= new File(copyfolder,fullname);
		return minisharefile;
	}
	
	public String getMiniHUBPasword() {
		return mMiniHUBPassword;
	}
	
	/**
	 * One check at a time
	 * @throws InterruptedException 
	 */
	public synchronized boolean checkMiniHUBPasword(String zPassword) throws InterruptedException {
		
		if(GeneralParams.MDS_PASSWORD.equals("")) {
			boolean valid = mMiniHUBPassword.replace("-", "").equalsIgnoreCase(zPassword.replace("-", "").trim());
			if(!valid) {
				//PAUSE - this prevents fast checking of passwords
				Thread.sleep(1000);
			}
			
			return valid;
		}
		
		boolean valid = mMiniHUBPassword.equals(zPassword.trim());
		if(!valid) {
			Thread.sleep(1000);
		}
		
		return valid;
	}
	
	public MiniDAPP getMiniDAPP(String zMiniDAPPID) {
		
		//Is it the Public
		if(zMiniDAPPID == mPublicMiniUID) {
			return mPublicMiniDAPP;
		}
		
		//Check the DB
		return MinimaDB.getDB().getMDSDB().getMiniDAPP(zMiniDAPPID);
	}
	
	public MiniDAPP getMiniDAPPFromName(String zName) {
		
		//Check the Public..
		if(zName.equalsIgnoreCase(mPublicMiniDAPP.getName())) {
			return mPublicMiniDAPP;
		}
		
		//Serach the DB
		ArrayList<MiniDAPP> allmini = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
		for(MiniDAPP mini : allmini) {
			if(mini.getName().equalsIgnoreCase(zName)) {
				return mini;
			}
		}
		
		return null;
	}
	
	/**
	 * Get the public MiniDAPP SessionID
	 */
	public String getPublicMiniDAPPSessionID() {
		return mPublicMiniSessionID;
	}
	
	public String getPublicMiniDAPPID() {
		return mPublicMiniUID;
	}
	
	/**
	 * Return the MINIDAPPID for a given SESSIONID
	 */
	public String convertSessionID(String zSessionID) {
		
		//Is it the Public..
		if(zSessionID.equals(mPublicMiniSessionID)) {
			return mPublicMiniUID;
		}
		
		return mSessionID.get(zSessionID);
	}
	
	/**
	 * Return the SESSIONID for a given MINIDAPPID
	 */
	public String convertMiniDAPPID(String zMiniDAPPID) {
		
		//Is it the Public..
		if(zMiniDAPPID.equals(mPublicMiniUID)) {
			return mPublicMiniSessionID;
		}
		
		//Search the rest
		Enumeration<String> keys = mSessionID.keys();
		while(keys.hasMoreElements()) {
			String sessionid 	= keys.nextElement();
			String minidapp 	= mSessionID.get(sessionid);
			if(minidapp.equals(zMiniDAPPID)) {
				return sessionid;
			}
		}
		
		return "";
	}
	
	public String addPendingCommand(MiniDAPP zMiniDAPP, String zCommand) {
		
		//Create a new pending command
		PendingCommand pc = new PendingCommand(zMiniDAPP.toJSON(), zCommand);
		
		//New Pending Command
		mPending.add(pc);
		
		return pc.getUID();
	}
	
	public ArrayList<PendingCommand> getAllPending(){
		return mPending;
	}
	
	public PendingCommand getPendingCommand(String zUID) {
		for(PendingCommand pending : mPending) {
			if(pending.getUID().equals(zUID)) {
				return pending;
			}
		}
		
		return null;
	}
	
	public boolean removePending(String zUID) {
		ArrayList<PendingCommand> newpending = new ArrayList<>();
		boolean found = false;
		for(PendingCommand pending : mPending) {
			if(!pending.getUID().equals(zUID)) {
				newpending.add(pending);
			}else {
				found = true;
			}
		}

		//Switch
		mPending = newpending;
		
		return found;
	}
	
	public void setMDSKeyPair(String zMiniDAPPID, String zKey, String zValue) {
		
		//Synchronise all access
		synchronized (mKeyPairSyncObject) {
			
			//The file
			File jsondbfile = new File(getMiniDAPPKeyPairFolder(zMiniDAPPID),"keypair.db");
			
			//Have we loaded it already..
			JsonDB jsondb = mKeyPairDB.get(zMiniDAPPID);
			
			//Does it exist
			if(jsondb == null) {
				
				//Create
				jsondb = new JsonDB();
				
				//Load it..
				if(jsondbfile.exists()) {
					jsondb.loadDB(jsondbfile);
				}
				
				//And add to our list
				mKeyPairDB.put(zMiniDAPPID, jsondb);
			}
			
			//Now set the Property
			jsondb.setString(zKey, zValue);
			
			//And save it..
			jsondb.saveDB(jsondbfile);
		}
	}
	
	public String getMDSKeyPair(String zMiniDAPPID, String zKey) {
		
		//Synchronise all access
		synchronized (mKeyPairSyncObject) {
			
			//The file
			File jsondbfile = new File(getMiniDAPPKeyPairFolder(zMiniDAPPID),"keypair.db");
			
			//Have we loaded it already..
			JsonDB jsondb = mKeyPairDB.get(zMiniDAPPID);
			
			//Does it exist
			if(jsondb == null) {
				
				//Create
				jsondb = new JsonDB();
				
				//Load it..
				if(jsondbfile.exists()) {
					jsondb.loadDB(jsondbfile);
				}
				
				//And add to our list
				mKeyPairDB.put(zMiniDAPPID, jsondb);
			}
			
			//Now get the Property
			return jsondb.getString(zKey);
		}
	}
	
	public JSONObject runSQL(String zUID, String zSQL) {
		
		//Are we shutting down..
		if(Main.getInstance() != null && Main.getInstance().isShuttongDownOrRestoring()) {
		//if(isShuttingDown()) {
			JSONObject err = new JSONObject();
			err.put("sql", zSQL);
			err.put("status", false);
			err.put("err", "Shutting down / Restoring..");
			return err;
		}
		
		//The MiniDAPPID
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
				db = new MiniDAPPDB(zUID);
				
				//The location
				File dbfolder3 = getMiniDAPPSQLFolder(minidappid);
				if(!dbfolder3.exists()) {
					dbfolder3.mkdirs();
				}
				
				try {
					
					//Now create the actual sql db
					db.loadDB(new File(dbfolder3,"sqldb"));
				
				} catch (SQLException e) {
					MinimaLogger.log(e);
					
					JSONObject err = new JSONObject();
					err.put("sql", zSQL);
					err.put("status", false);
					err.put("err", e.toString());
					return err;
				}
				
				//Add to the List
				mSqlDB.put(minidappid, db);
			}
		}
		
		//Now run the SQL
		JSONObject res = db.executeSQL(zSQL);
		
		return res;
	}
	
	public void addAPICall(APICallback zAPICallback) {
		mAPICalls.add(zAPICallback);
	}
	
	public APICallback getAPICallback(String zRandID) {
		APICallback foundapicall = null;
		for(APICallback api : mAPICalls) {
			if(api.getRandID().equals(zRandID)) {
				foundapicall = api;
				break;
			}
		}
		
		//Did we find it..
		if(foundapicall != null) {
			mAPICalls.remove(foundapicall);
		}
		
		return foundapicall;
	}
	
	public void shutdownSQL(String zMiniDAPPID){
		//The final DB
		MiniDAPPDB db = mSqlDB.get(zMiniDAPPID);
		
		if(db != null) {
			db.saveDB(true);
		}
		
		mSqlDB.remove(zMiniDAPPID);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Is it even enabled
		if(!GeneralParams.MDS_ENABLED) {
			return;
		}
		
		if(zMessage.getMessageType().equals(MDS_INIT)) {
			
			//SELF Signed Cert or use your own..
			if(GeneralParams.MDS_NOSSL) {
				
				mMDSFileServer = new HTTPServer(GeneralParams.MDSFILE_PORT) {
					@Override
					public Runnable getSocketHandler(Socket zSocket) {
						return new MDSFileHandler( getWebFolder() , zSocket, MDSManager.this,mPollStack);
					}
				};
				
			}else {
				//Create an SSL server
				mMDSFileServer = new HTTPSServer(GeneralParams.MDSFILE_PORT) {
					
					@Override
					public Runnable getSocketHandler(SSLSocket zSocket) {
						return new MDSFileHandler( getWebFolder() , zSocket, MDSManager.this,mPollStack);
					}
				};
			}
			
			//The MDS Password
			if(GeneralParams.MDS_PASSWORD.equals("")) {
				//Create a NEW Main Password..
				MiniData password 	= MiniData.getRandomData(64);
				String b32			= BaseConverter.encode32(password.getBytes());
				
				mMiniHUBPassword	= b32.substring(2,6)+"-"
									 +b32.substring(7,11)+"-"
									 +b32.substring(12,16)+"-"
									 +b32.substring(17,21);
			
			}else {
				//Pre-set..
				mMiniHUBPassword	= GeneralParams.MDS_PASSWORD;
			}
			
			//Is there a Foler of DAPPs to be installed..
			if(!GeneralParams.MDS_INITFOLDER.equals("") && !MinimaDB.getDB().getUserDB().getMDSINIT()) {
				
				//Scan that folder..
				File[] dapps = new File(GeneralParams.MDS_INITFOLDER).listFiles();
				if(dapps!=null) {
					
					//Cycle through..
					for(File dapp : dapps) {
						installMiniDAPP(dapp, GeneralParams.MDS_WRITE);
					}
				}
				
				//Ok we have done it now..
				MinimaDB.getDB().getUserDB().setMDSINIT(true);
				MinimaDB.getDB().saveUserDB();
			}
			
			//Set up the RHINOJS ContextFactory
			//Weird here when Android doesn't clear the class and the static variable persists..
			if(!ContextFactory.hasExplicitGlobal()) {
				ContextFactory.initGlobal(new SandboxContextFactory());
			}else {
				MinimaLogger.log("MDS RHINOJS INIT hasGlobal Allready!.. may need a restart");
			}
			
			//Create the Public SessionID
			if(GeneralParams.PUBLICMDS_SESSION_UID.equals("")) {
				mPublicMiniSessionID = MiniData.getRandomData(128).to0xString();
			}else {
				mPublicMiniSessionID = new String(GeneralParams.PUBLICMDS_SESSION_UID);
			}
			
			//Install the default MiniHUB..
			doDefaultMiniHUB();
			
			//Scan for MiniDApps
			PostMessage(MDS_MINIDAPPS_RESETALL);
		
			//Post another Message
			PostTimerMessage(new TimerMessage(10000, MDS_TIMER_10SECONDS));
			PostTimerMessage(new TimerMessage(60000, MDS_TIMER_60SECONDS));
			PostTimerMessage(new TimerMessage(60000 * 60, MDS_TIMER_1HOUR));
			
		}else if(zMessage.getMessageType().equals(MDS_SHUTDOWN)) {

			//Notify SQL calls not to happen
			mIsShuttingDown = true;
			
			//Shutdown the Runnables
			MinimaLogger.log("Shutdown MDS runnables..");
			for(MDSJS mds : mRunnables) {
				try {
					mds.shutdown();
				}catch(Exception exc) {
					MinimaLogger.log(exc);
				}
			}
			
			//Shut down the servers
			MinimaLogger.log("Shutdown MDS File and Command servers..");
			if(GeneralParams.MDS_ENABLED) {
				mMDSFileServer.shutdown();
			}
			
			//Save all the DBs
			MinimaLogger.log("Shutdown MDS databases..");
			Enumeration<MiniDAPPDB> dbs = mSqlDB.elements();
			while(dbs.hasMoreElements()) {
				dbs.nextElement().saveDB(false);
			}
			
			stopMessageProcessor();
			
		}else if(zMessage.getMessageType().equals(MDS_TIMER_10SECONDS)) {

			//Create a datat object
			JSONObject data = new JSONObject();
			data.put("timemilli", Long.toString(System.currentTimeMillis()));
			
			//Send a POLL message.. 
			Main.getInstance().PostNotifyEvent(MDS_TIMER_10SECONDS, data);
			
			//Post another Message
			PostTimerMessage(new TimerMessage(10000, MDS_TIMER_10SECONDS));
			
		}else if(zMessage.getMessageType().equals(MDS_TIMER_60SECONDS)) {

			//Create a datat object
			JSONObject data = new JSONObject();
			data.put("timemilli", Long.toString(System.currentTimeMillis()));
			
			//Send a POLL message.. 
			Main.getInstance().PostNotifyEvent(MDS_TIMER_60SECONDS, data);
			
			//Post another Message
			PostTimerMessage(new TimerMessage(60000, MDS_TIMER_60SECONDS));
			
		}else if(zMessage.getMessageType().equals(MDS_TIMER_1HOUR)) {

			//Create a datat object
			JSONObject data = new JSONObject();
			data.put("timemilli", Long.toString(System.currentTimeMillis()));
			
			//Send a POLL message.. 
			Main.getInstance().PostNotifyEvent(MDS_TIMER_1HOUR, data);
			
			//Post another Message
			PostTimerMessage(new TimerMessage(60000 * 60, MDS_TIMER_1HOUR));
			
		}else if(zMessage.getMessageType().equals(MDS_POLLMESSAGE)) {

			// Add a message to the POll..
			JSONObject poll = (JSONObject) zMessage.getObject("poll");
			String to 		= zMessage.getString("to");
			
			//Check for shutdown message - sent at the end so all other messages must have been processed
			if(poll.getString("event").equals("MDS_SHUTDOWN")) {
				MinimaLogger.log("JS RUNNABLES received all POLL messages.. SHUTDOWN started..");
			}
			
			boolean sendtoall = true;
			if(poll.getString("event").equals("MDSAPI")) {
				
				//Get the data
				JSONObject dataobj = (JSONObject) poll.get("data");
				
				//Is it  a response..
				if(!(boolean)dataobj.get("request")) {
					
					//RESPONSE messages are not forwarded
					sendtoall = false;
					
					//Send to the API Call..
					APICallback api = getAPICallback(dataobj.getString("id"));
					if(api != null) {
						
						//Construct a reply..
						JSONObject reply = new JSONObject();
						reply.put("status", dataobj.get("status"));
						reply.put("data", dataobj.get("message"));
						
						//Call it..
						Object[] args = { NativeJSON.parse(api.getContext(), 
									api.getScope(),reply.toString(), new NullCallable()) };
						
						//Call the main MDS Function in JS
						api.getFunction().call(api.getContext(), api.getScope(), api.getScope(), args);
						
					}else {
						//MinimaLogger.log("RUNNABLE NOT FOUND API CALL : "+dataobj.toString());
					}
				}
			}
			
			//Send message to the runnables first..
			if(sendtoall) {
				for(MDSJS mds : mRunnables) {
					try {
						
						if(to.equals("*")) {
							//Send to the runnable
							mds.callMainCallback(poll);
						}else {
							
							//Check the MiniDAPPID
							if(mds.getMiniDAPPID().equals(to)) {
								//Send to the runnable
								mds.callMainCallback(poll);
							}
						}
						
					}catch(Exception exc) {
						MinimaLogger.log(exc, false);
					}
				}
			}
			
			//Add then to the Poll Stack - web minidapps
			mPollStack.addMessage(poll,to);
		
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_RESETSESSIONS)) {
			
			//Clear the Old
			mSessionID.clear();
			
			//Reassign..
			ArrayList<MiniDAPP> dapps = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
			for(MiniDAPP dapp : dapps) {
				//Use a 128 random value..  
				String sessionid = MiniData.getRandomData(128).to0xString();
				mSessionID.put(sessionid, dapp.getUID());
			}
			
			//Something has changed
			PostMiniDAPPChange();
			
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_RESETALL)) {
			
			//Shut down all the Context Objkects..
			for(MDSJS mds : mRunnables) {
				mds.shutdown();
			}
			
			//Now clear
			mRunnables.clear();
			mSessionID.clear();
			
			//Scan through and see what we have..
			ArrayList<MiniDAPP> dapps = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
			for(MiniDAPP dapp : dapps) {
				
				//Set it up
				setupMiniDAPP(dapp);
			}
		
			mHasStarted = true;
		
			//Something has changed
			PostMiniDAPPChange();
			
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_INSTALLED)) {
			
			//Get the MiniDAPP
			MiniDAPP dapp = (MiniDAPP) zMessage.getObject("minidapp");
				
			//Install it..
			setupMiniDAPP(dapp);
		
			//Something has changed
			PostMiniDAPPChange();
			
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_UNINSTALLED)) {
			
			//Remove a MiniDAPP
			String uid = zMessage.getString("uid");
			
			//First remove the Runnable
			ArrayList<MDSJS> runnables = new ArrayList();
			for(MDSJS mds : mRunnables) {
				if(mds.getMiniDAPPID().equals(uid)) {
					mds.shutdown();
				}else {
					runnables.add(mds);
				}
			}
			
			//And switch the list over..
			mRunnables = runnables;
			
			//And now remove the sessionid
			mSessionID.remove(convertMiniDAPPID(uid));
			
			//Something has changed
			PostMiniDAPPChange();
		}
	}

	/**
	 * MiniDAPP installed uninstalled or sessions changed
	 */
	public void PostMiniDAPPChange() {
		Main.getInstance().PostNotifyEvent("MDS_MINIDAPPS_CHANGE", new JSONObject());
	}
	
	/**
	 * Initialise a MiniDAPP
	 */
	private void setupMiniDAPP(MiniDAPP zDAPP) {
		
		//Add a unique random SessionID
		String sessionid = MiniData.getRandomData(128).to0xString();
		mSessionID.put(sessionid, zDAPP.getUID());
		
		//Is there a service.js class
		File service = new File(getMiniDAPPWebFolder(zDAPP.getUID()),"service.js");
		if(service.exists()) {
			
			try {
				MinimaLogger.log("Start Service "+zDAPP.getName());
				
				//Load the file..
				byte[] serv = MiniFile.readCompleteFile(service);
				String code = new String(serv,MiniString.MINIMA_CHARSET);
				
				//Load it into the service runner..
				Context ctx = Context.enter();
				ctx.setOptimizationLevel(-1);
				ctx.setLanguageVersion(Context.VERSION_ES6);
				ctx.setMaximumInterpreterStackDepth(1024);
				
				//Stop JAVA classes from being run..
				try {
					ctx.setClassShutter(new ClassShutter() {
						public boolean visibleToScripts(String className) {					
							
							//ONLY MDSJS can be called form JS
							if(className.startsWith("org.minima.system.mds.runnable")) {
								return true;
							}
								
							//MinimaLogger.log("RHINOJS JAVA CLASS DENIED ACCESS : "+className);
							
							return false;
						}
					});
				}catch(SecurityException sec) {
					if(sec.getMessage().equals("Cannot overwrite existing ClassShutter object")) {
						//we already set it..
					}else {
						MinimaLogger.log(sec);
					}
				}
				
				//Create the Scope
				Scriptable scope = ctx.initStandardObjects();
				
				//Create an MDSJS object
				MDSJS mdsjs = new MDSJS(this, zDAPP.getUID(), zDAPP.getName(), ctx, scope);
				ScriptableObject.putProperty(scope, "MDS", Context.javaToJS(mdsjs, scope));
				
				//Add the main code to the Runnable
				ctx.evaluateString(scope, code, "<mds_"+zDAPP.getUID()+">", 1, null);
			
				//Add to our list
				mRunnables.add(mdsjs);
			
			}catch(Exception exc) {
				MinimaLogger.log("ERROR starting service "+zDAPP.getName()+" "+exc);
			}
		}
	}
	
	/**
	 * Install a MiniDAPP file
	 */
	public boolean installMiniDAPP(File zMiniDAPP, String zWriteAccess) {		
	
		if(!zMiniDAPP.isFile()) {
			return false;
		}
		
		if(!zMiniDAPP.exists()) {
			MinimaLogger.log("MiniDAPP @ "+zMiniDAPP.getAbsolutePath()+" does not exist..");
			return false;
		}
		
		//Now start
		try {
			FileInputStream fis = new FileInputStream(zMiniDAPP);
		
			//Where is it going..
			String rand = MiniData.getRandomData(32).to0xString();
			
			//The file where the package is extracted..
			File dest 	= new File(getWebFolder(),rand);
			if(dest.exists()) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
			dest.mkdirs();
			
			//Send it to the extractor..
			ZipExtractor.unzip(fis, dest);
			fis.close();
			
			//Is there a conf file..
			File conf = new File(dest,"dapp.conf");
			if(!conf.exists()) {
				
				MinimaLogger.log("MiniDAPP @ "+zMiniDAPP.getAbsolutePath()+" no conf file..");
				
				//Delete the install
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);	
				
				return false;
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//Is this one set to write
			if(!zWriteAccess.equals("") && jsonconf.containsKey("name")) {
				if(jsonconf.getString("name").equals(zWriteAccess)){
					MinimaLogger.log(jsonconf.getString("name","")+" MiniDAPP set to WRITE access");
					jsonconf.put("permission", "write");
				}else {
					//ALWAYS starts with only READ Permission
					jsonconf.put("permission", "read");
				}
			}else {
				//ALWAYS starts with only READ Permission
				jsonconf.put("permission", "read");
			}
			
			//Create the MiniDAPP
			MiniDAPP md = new MiniDAPP(rand, jsonconf);
			
			//Now add to the DB
			MinimaDB.getDB().getMDSDB().insertMiniDAPP(md);
			
			MinimaLogger.log("MiniDAPP @ "+zMiniDAPP.getAbsolutePath()+" installed..");
			
		} catch (Exception e) {
			MinimaLogger.log(e);
			return false;
		}
		
		return true;
	}
	
	/**
	 * The Default MiniHUB is updated every time you start..
	 */
	private void doDefaultMiniHUB() throws Exception {
		
		//The main MDS DB
		MDSDB mdb = MinimaDB.getDB().getMDSDB();
		
		//Do we have a MiniHUB installed..
		DEFAULT_MINIHUB = MinimaDB.getDB().getUserDB().getDefaultMiniHUB();
		
		//And install some default dapps..
		ArrayList<MiniDAPP> allminis = mdb.getAllMiniDAPPs();
				
		//Check for HUB
		checkInstalled("minihub", "minihub/minihub-0.19.1.mds.zip", allminis, true, true);
		
		//Do we Install the Default MiniDAPPs
		if(GeneralParams.DEFAULT_MINIDAPPS) {
		
			//Pending gets write permissions
			checkInstalled("pending", "default/pending-1.2.0.mds.zip", allminis, true);
			
			//Security MiniDAPP - backups / restore
			checkInstalled("security", "default/security-1.10.1.mds.zip", allminis, true);
			
			//Dappstore gets write permissions
			checkInstalled("dapp store", "default/dapp_store-1.0.8.mds.zip", allminis, true);
			
			//The rest are normal
			checkInstalled("block", "default/block-3.2.0.mds.zip", allminis, false);
			checkInstalled("chatter", "default/chatter-1.10.4.mds.zip", allminis, false);
			checkInstalled("docs", "default/docs-2.1.0.mds.zip", allminis, false);
			checkInstalled("ethwallet", "default/ethwallet-1.9.4.mds.zip", allminis, false);
			checkInstalled("filez", "default/filez-1.9.4.mds.zip", allminis, false);
			checkInstalled("future cash", "default/futurecash-2.7.1.mds.zip", allminis, false);
			checkInstalled("health", "default/health-1.1.5.mds.zip", allminis, false);
			checkInstalled("logs", "default/logs-1.0.2.mds.zip", allminis, false);
			checkInstalled("maxcontacts", "default/maxcontacts-1.14.0.mds.zip", allminis, false);
			checkInstalled("maximize", "default/maximize-1.3.0.mds.zip", allminis, false);
			checkInstalled("maxsolo", "default/maxsolo-2.7.2.mds.zip", allminis, false);
			//checkInstalled("megawallet", "default/megawallet-1.5.0.mds.zip", allminis, false);
			checkInstalled("miniswap", "default/miniswap-2.15.5.mds.zip", allminis, false);
			checkInstalled("news feed", "default/news-2.0.mds.zip", allminis, false);
			checkInstalled("script ide", "default/scriptide-2.1.1.mds.zip", allminis, false);
			checkInstalled("shout out", "default/shoutout-1.4.0.mds.zip", allminis, false);
			checkInstalled("sql bench", "default/sqlbench-0.6.mds.zip", allminis, false);
			checkInstalled("terminal", "default/terminal-2.3.2.mds.zip", allminis, false);
			checkInstalled("the safe", "default/thesafe-1.7.0.mds.zip", allminis, false);
			checkInstalled("vestr", "default/vestr-1.8.1.mds.zip", allminis, false);
			checkInstalled("wallet", "default/wallet-2.46.6.mds.zip", allminis, false);
		}
	}
	
	private String getVersionFromPath(String zPath) {
		
		//Find the numbers..
		int start = zPath.indexOf("-");
		if(start == -1) {
			return "0";
		}
		
		int end = zPath.indexOf(".mds.zip");
		if(end == -1) {
			return "0";
		}
		
		//Chop it..
		return zPath.substring(start+1,end);
	}
	
	private boolean checkInstalled(String zName, String zResource,  ArrayList<MiniDAPP> zAllDapps, boolean zWrite) {
		return checkInstalled(zName, zResource, zAllDapps, zWrite, false);
	}
	
	private boolean checkInstalled(String zName, String zResource,  ArrayList<MiniDAPP> zAllDapps, boolean zWrite, boolean zIsMiniHUB) {		
		
		//The main MDS DB
		MDSDB mdb = MinimaDB.getDB().getMDSDB();
		
		//Check if Uninstalled - so do not re-install
		if(MinimaDB.getDB().getUserDB().checkUninstalledMiniDAPP(zName)) {
			MinimaLogger.log("Default MiniDAPP "+zName+" uninstalled - not re-installing..");
			return true;
		}
		
		try {
			
			//Is it already installed
			for(MiniDAPP md : zAllDapps) {
				if(md.getName().equalsIgnoreCase(zName)) {
					
					//Check the Version..
					String newversion = getVersionFromPath(zResource);
					String oldversion = md.getVersion();
					
					//Is it newer
					if(Maths.compareVersions(newversion, oldversion)>0) {
						
						//Update this MiniDAPP..
						updateMiniHUB(zResource, md.getUID(), zWrite);
					}
					
					//Check if this is the MiniHUB..
					if(zName.equals("minihub")) {
						
						//Is it correct
						if(!DEFAULT_MINIHUB.equals(md.getUID())) {
							DEFAULT_MINIHUB = md.getUID();
							
							//And set in UserDB..
							MinimaDB.getDB().getUserDB().setDefaultMiniHUB(DEFAULT_MINIHUB);
							MinimaDB.getDB().saveUserDB();
						}
						
						//Always set MiniHUB to WRITE
						MiniDAPP minihubmd = mdb.getMiniDAPP(DEFAULT_MINIHUB);
						minihubmd.setPermission("write");
						mdb.deleteMiniDAPP(DEFAULT_MINIHUB);
						mdb.insertMiniDAPP(minihubmd);
					}
					
					return true;
				}
			}
			
			//Ok - Install it..
			installDefaultMiniDAPP(zResource,zWrite,zIsMiniHUB);
			
		}catch(Exception exc) {
			MinimaLogger.log("[!] Failed install of "+zName+" @ "+zResource);			
		}
		
		return false;
	}
	
	private void installDefaultMiniDAPP(String zResource, boolean zWrite, boolean zIsMiniHUB) {
		
		//The MiniHUB
		String minidapp = zResource;
		File dest 		= null;
		
		try {
			
			//Get the MiniHUB file..
			InputStream is 	= getClass().getClassLoader().getResourceAsStream(minidapp);
			
			//Get all the data..
			byte[] alldata = MiniFile.readAllBytes(is);
			is.close();
			
			//Create an input stream for the file..
			ByteArrayInputStream bais 	= new ByteArrayInputStream(alldata);
			
			//Where is it going..
			String rand = MiniData.getRandomData(32).to0xString();
			
			//The file where the package is extracted..
			dest 	= new File(getWebFolder(), rand);
			if(dest.exists()) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
			boolean mk = dest.mkdirs();
		
			//Send it to the extractor..
			ZipExtractor.unzip(bais, dest);
			bais.close();
			
			//Is there a conf file..
			File conf = new File(dest,"dapp.conf");
			if(!conf.exists()) {
				throw new Exception("No dapp.conf file found @ "+conf.getAbsolutePath());
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//ALWAYS starts with only READ Permission
			if(zWrite) {
				jsonconf.put("permission", "write");
			}else {
				jsonconf.put("permission", "read");
			}
			
			//Which version..
			String name		= jsonconf.getString("name");
			String version 	= jsonconf.getString("version");
			MinimaLogger.log("Installing default MiniDAPP.. "+name+" v"+version);
			
			//Create the MiniDAPP
			MiniDAPP md = new MiniDAPP(rand, jsonconf);
			
			//Now add to the DB
			MDSDB db = MinimaDB.getDB().getMDSDB();
			db.insertMiniDAPP(md);
		
			//Now copy the minidapp itself..so you have a copy..
			File copyfolder = Main.getInstance().getMDSManager().getMiniDAPPCopyDappFolder(md.getUID());
			MiniFile.deleteFileOrFolder(copyfolder.getAbsolutePath(), copyfolder);
			copyfolder.mkdirs();
			File minisharefile 	= getMiniDAPPShareFile(md);
			try {
				MiniFile.writeDataToFile(minisharefile, alldata);
			}catch(Exception Exc) {
				MinimaLogger.log(Exc);
			}
			
			if(zIsMiniHUB) {
				//Create the webpage
				DEFAULT_MINIHUB = rand;
				
				//And set in UserDB..
				MinimaDB.getDB().getUserDB().setDefaultMiniHUB(rand);
				MinimaDB.getDB().saveUserDB();
			}
			
		}catch(Exception exc) {
			
			//Can log this..
			MinimaLogger.log("[!] Failed install of "+zResource);
			MinimaLogger.log(exc);
			
			//Delete the install
			if(dest != null) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
		}
	}
	
	private void updateMiniHUB(String zResource, String zMiniDAPPID, boolean zWrite) {
		
		File minidapp = null;
		
		try {
					
			//Get the MiniHUB file..
			InputStream is = getClass().getClassLoader().getResourceAsStream(zResource);
			
			//Get all the data..
			byte[] alldata = MiniFile.readAllBytes(is);
			is.close();
			
			//Create an input stream for the file..
			ByteArrayInputStream bais 	= new ByteArrayInputStream(alldata);
			
			//Now the MiniDAPP ID
			MDSDB db 			= MinimaDB.getDB().getMDSDB();
			MiniDAPP md 		= db.getMiniDAPP(zMiniDAPPID);
			
			//Get the Conf..
			JSONObject miniconf = md.getConfData();
			
			//Delete ONLY the old WEB files
			String mdsroot 	= getRootMDSFolder().getAbsolutePath();
			minidapp 		= new File(getWebFolder(),zMiniDAPPID);
			if(minidapp.exists()) {
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);
			}
			
			//Extract the new files.. make sure exists
			minidapp.mkdirs();
			
			//Send it to the extractor..
			ZipExtractor.unzip(bais, minidapp);
			bais.close();
		
			//Is there a conf file..
			File conf = new File(minidapp,"dapp.conf");
			if(!conf.exists()) {
				
				//Delete the install
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);	
				
				throw new Exception("No dapp.conf file found");
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//Is it already in write mode..
			String permission = md.getPermission();
			
			//Copy the trust
			if(zWrite) {
				jsonconf.put("permission", "write");
			}else {
				jsonconf.put("permission", permission.toLowerCase());
			}
			
			//Which version..
			String version = jsonconf.getString("version");
			MinimaLogger.log("Updating default MiniDAPP.. "+jsonconf.getString("name")+" to v"+version);
			
			//Delete the old..
			db.deleteMiniDAPP(zMiniDAPPID);
			
			//The NEW miniDAPP
			MiniDAPP newmd = new MiniDAPP(zMiniDAPPID, jsonconf);
			
			//Now add to the DB
			db.insertMiniDAPP(newmd);
			
			//Now copy the minidapp itself..so you have a copy..
			File copyfolder = Main.getInstance().getMDSManager().getMiniDAPPCopyDappFolder(newmd.getUID());
			MiniFile.deleteFileOrFolder(copyfolder.getAbsolutePath(), copyfolder);
			copyfolder.mkdirs();
			File minisharefile 	= getMiniDAPPShareFile(newmd);
			try {
				MiniFile.writeDataToFile(minisharefile, alldata);
			}catch(Exception Exc) {
				MinimaLogger.log(Exc);
			}
			
		}catch(Exception exc) {
			
			//Can log this..
			MinimaLogger.log("[!] Failed update of "+zResource);
			MinimaLogger.log(exc);

			if(minidapp != null) {
				//Delete the install
				MiniFile.deleteFileOrFolder(minidapp.getAbsolutePath(), minidapp);
			}
		}
	}
	
	public String getDefaultMiniHUB() {
		return DEFAULT_MINIHUB;
	}
}
