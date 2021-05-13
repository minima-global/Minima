package org.minima.system.network.minidapps;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.commands.SQL;
import org.minima.system.network.minidapps.comms.CommsManager;
import org.minima.system.network.minidapps.minibackend.BackEndDAPP;
import org.minima.system.network.minidapps.websocket.WebSocketManager;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SQLHandler;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.minima.utils.nanohttpd.protocols.http.NanoHTTPD;

public class DAPPManager extends MessageProcessor {

	public static String DAPP_INIT           = "DAPP_INIT";
	public static String DAPP_INSTALL        = "DAPP_INSTALL";
	public static String DAPP_UNINSTALL      = "DAPP_UNINSTALL";
	public static String DAPP_RELOAD         = "DAPP_RELOAD";
	
	public static String DAPP_DIRECTPOST      = "DAPP_DIRECTPOST";
	public static String DAPP_DIRECTREPLY     = "DAPP_DIRECTREPLY";
	
	public static String DAPP_MINIDAPP_POST     = "DAPP_MINIDAPP_POST";
	public static String DAPP_MINIDAPP_POSTALL  = "DAPP_MINIDAPP_POSTALL";
	
	JSONArray CURRENT_MINIDAPPS = new JSONArray();
	String MINIDAPPS_FOLDER     = "";
	
	//The MiniDAPP app server
	DAPPServer mDAPPServer;
	
	//The CommsManager for all the MIniDAPPS
	CommsManager mCommsManager; 
	
	//The Edited minima.js file..
	byte[] mMINIMAJS = new byte[0];
	
	NetworkHandler mNetwork;
	
	/**
	 * BackEnd JS DAPP
	 */
	Hashtable<String, BackEndDAPP> mBackends;
	
	//The List of Post messages for Replies..
	Hashtable<String, Message> mReplyMessage;
	long mLastReplyUsed = 0; 
		
	public DAPPManager() {
		super("DAPPMAnager");
		
		//Need access to this
		mNetwork = Main.getMainHandler().getNetworkHandler();
		
		//All the backends are stored here..
		mBackends = new Hashtable<>();
		
		//ReplyID to a MiniDAPP request
		mReplyMessage = new Hashtable<>();
		
		//Init the System
		PostMessage(DAPP_INIT);
	}
	
	public CommsManager getCommsManager() {
		return mCommsManager;
	}
	
	public NetworkHandler getNetworkHandler() {
		return mNetwork;
	}
	
	public void stop() {
		if(mDAPPServer != null) {
			mDAPPServer.stop();
		}
		
		if(mCommsManager != null) {
			mCommsManager.shutdown();
		}
		
		stopMessageProcessor();
	}
	
	public JSONArray getMiniDAPPS() {
		//Has the HOST changed..
		String host    = getNetworkHandler().getBaseHost();
		String newhost = getNetworkHandler().calculateHostIP();
		if(!host.equals(newhost)) {
			//Recalculate
			recalculateMiniDAPPS();	
		}
		
		return CURRENT_MINIDAPPS;
	}
	
	public String getMiniDAPPSFolder() {
		return MINIDAPPS_FOLDER;
	}
	
	private JSONObject loadConfFile(File zConf) {
		JSONObject ret = new JSONObject();
		
		//Some details..
		String root = zConf.getParent();
        int start = root.indexOf("minidapps");
        String uid = root.substring(start+10);
        
		StringBuilder tot = new StringBuilder();
		try {
			FileInputStream fis     = new FileInputStream(zConf);
			InputStreamReader is    = new InputStreamReader(fis);
			BufferedReader bis      = new BufferedReader(is);
			
			String sCurrentLine;
	        while ((sCurrentLine = bis.readLine()) != null) {
	        	tot.append(sCurrentLine).append("\n");
	        }
	        
	        //Now convert..
	        JSONParser parser = new JSONParser();
	        ret = (JSONObject) parser.parse(tot.toString());
	        
	        //Add defaults..
	        ret.put("uid", uid);
	        ret.put("root", "/minidapps/"+uid);
	        ret.put("web", "http://"+mNetwork.getBaseHost()+":"+mNetwork.getMiniDAPPServerPort()+"/minidapps/"+uid);
	        
	        bis.close();
	        fis.close();
	        
		} catch (Exception e) {
			MinimaLogger.log("Error Loading MiniDAPP conf "+zConf.getAbsolutePath(),e);
			//Extra Info..
			String totstr = tot.toString();
			if(!totstr.equals("")) {
				MinimaLogger.log(tot.toString());
			}
			
			//Clear it.. but add details required to remove it..
			ret = new JSONObject();
			ret.put("name", "*ERROR*");
			ret.put("description", "This minidapp did not load correctly..");
			ret.put("uid", uid);
			ret.put("installed", (long)0);
			ret.put("version", "1.0");
	        ret.put("root", "");
	        ret.put("web", "http://"+mNetwork.getBaseHost()+":"+mNetwork.getMiniDAPPServerPort()+"/minidapps/"+uid);
		}
		
		return ret;
	}
	
	private JSONArray recalculateMiniDAPPS() {
		MinimaLogger.log("Recalculate MiniDAPPS @ "+mNetwork.getBaseHost());
		
		//Clear the OLD
		CURRENT_MINIDAPPS.clear();
		
		//Close the SQL DBs..
		SQLHandler.CloseSQL();
		
		//And the backends..
		Enumeration<BackEndDAPP> bends = mBackends.elements();
		while(bends.hasMoreElements()) {
			BackEndDAPP bend = bends.nextElement();
			bend.shutdown();
		}
		mBackends.clear();
		
		//This is the folder..
		File alldapps = Main.getMainHandler().getBackupManager().getMiniDAPPFolder();
		
		//Store for later
		MINIDAPPS_FOLDER = alldapps.getAbsolutePath();
		
		//List it..
		File[] apps = alldapps.listFiles();
		
		//Cycle through them..
		if(apps != null) {
			for(File app : apps) {
				//Get the time stamp - when was it installed..
				long timestamp = System.currentTimeMillis();
				File tsfile = new File(app,"minimatimestamp");
				if(tsfile.exists()) {
					try {
						FileInputStream  tsfis = new FileInputStream(tsfile);
						DataInputStream dais = new DataInputStream(tsfis);
						timestamp = dais.readLong();
						dais.close();
						tsfis.close();
					} catch (IOException e) {
						MinimaLogger.log("Error loading timestamp file.. "+e);
					}
				}
				
				//What MiniDAPP is this..
				String minidappid = app.getName();
				
				//Find the .minidapp file..
				String download = "";
				File[] files = app.listFiles();
				if(files!=null) {
					for(File ff : files) {
						if(ff.getName().endsWith(".minidapp")) {
							//Found it..
							download = "http://"+mNetwork.getBaseHost()+":"+mNetwork.getMiniDAPPServerPort()+"/minidapps/"+minidappid+"/"+ff.getName(); 
							break;
						}
					}
				}
				
				//Open it up..
				File conf = new File(app,"minidapp.conf");
				File backend = new File(app,"service.js");
				
				//Check it exists..
				if(conf!=null && conf.exists()) {
					//Load it..
					JSONObject confjson = loadConfFile(conf);
					
					//Did it work..
					boolean error = false;
					String mininame = (String) confjson.get("name");
					if(mininame.equals("*ERROR*")) {
						//Something went wrong..
						error = true;
					}
						
					//Add the timestamp..
					confjson.put("installed", timestamp);
					confjson.put("download", download);
					
					//Is there a Back end
					if(!error && backend.exists()) {
						//Load it..
						try {
							//Load the JS file..
							String backjs = new String(MiniFile.readCompleteFile(backend),"UTF-8");
						
							String dappname = "no_name_in_conf"; 
							if(confjson.containsKey("name")) {
								dappname = (String)confjson.get("name");
							}
							
							//Create a BackEnd APP..
							BackEndDAPP bedapp = new BackEndDAPP(dappname, backjs, minidappid);
							
							//Add to the List..
							mBackends.put(minidappid, bedapp);
							
							
							MinimaLogger.log("BackEndJS create for "+dappname+" @ "+minidappid);
							
						} catch (Exception e) {
							MinimaLogger.log("Error loading service.js for "+backend.getAbsolutePath()+" "+e);
						} 
					}
					
					//Add it..
					CURRENT_MINIDAPPS.add(confjson);
					
				}else {
					MinimaLogger.log("ERROR : minidapp.conf not found for "+minidappid);
				
					//Some details..
					if(conf != null) {
						String root = conf.getParent();
				        int start = root.indexOf("minidapps");
				        
				        if(start != -1) {
					        String uid = root.substring(start+10);
				        
							JSONObject confjson = new JSONObject();
							confjson.put("name", "*ERROR*");
							confjson.put("description", "minidapp.conf file missing..");
							confjson.put("uid", uid);
							confjson.put("installed", (long)0);
							confjson.put("root", "");
							confjson.put("version", "1.0");
							confjson.put("web", "http://"+mNetwork.getBaseHost()+":"+mNetwork.getMiniDAPPServerPort()+"/minidapps/"+uid);
							
					        //Add it..
							CURRENT_MINIDAPPS.add(confjson);
				        }
					}
				}
			}
		}
		
		//Order the List.. By Name..
		Collections.sort(CURRENT_MINIDAPPS, new Comparator<JSONObject>() {
			@Override
			public int compare(JSONObject o1, JSONObject o2) {
				try {
					if(o1.containsKey("name") && o2.containsKey("name")){
						//In case the name is missing..
						String name1 = (String) o1.get("name");
						String name2 = (String) o2.get("name");	
						return name1.compareTo(name2);
					}
					
				}catch(Exception exc) {
					System.out.println("Error in MiniDAPP CONF "+exc);
				}
				
				return 0;
			}
		});
		
		return CURRENT_MINIDAPPS;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(DAPP_INIT)) {
			//Are we ready..
			if(!Main.getMainHandler().getConsensusHandler().isInitialSyncComplete()) {
				//Init the System - when this is done..
				PostTimerMessage(new TimerMessage(1000, DAPP_INIT));	
				return;
			}
			
			//Create the Comms Manager
			mCommsManager = new CommsManager(Main.getMainHandler());
			
			//Calculate the current MiniDAPPS
			recalculateMiniDAPPS();
			
			//We HAVE LIFT OFF!
			Main.getMainHandler().getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALSYNC));
			
			//Create the MiniDAPP server
			mDAPPServer = new DAPPServer(mNetwork.getMiniDAPPServerPort(), this);
			try {
				mDAPPServer.start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
				MinimaLogger.log("MiniDAPP server started on port "+mNetwork.getMiniDAPPServerPort());
				
			} catch (IOException e) {
				MinimaLogger.log("MiniDAPP server error "+ e.toString());
			}
			
		}else if(zMessage.getMessageType().equals(DAPP_RELOAD)) {
			//Recalculate the MINIDAPPS
			recalculateMiniDAPPS();
			
			//Get the response JSON
			JSONObject mdapps = InputHandler.getResponseJSON(zMessage);
			
			mdapps.put("cwd", new File("").getAbsolutePath());
			mdapps.put("count", CURRENT_MINIDAPPS.size());
			mdapps.put("minidapps", CURRENT_MINIDAPPS);
			InputHandler.endResponse(zMessage, true, "MiniDAPPs reloaded");
			
			Main.getMainHandler().getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_DAPP_RELOAD));
			
		}else if(zMessage.getMessageType().equals(DAPP_INSTALL)) {
			//Get the Data
			MiniData data = (MiniData) zMessage.getObject("minidapp");
			String filename = zMessage.getString("filename");
			
			MinimaLogger.log("INSTALLING : "+filename+" "+data.getLength());
			
			//Do we overwrite..
			boolean overwrite = true;
			if(zMessage.exists("overwrite")){
				overwrite = zMessage.getBoolean("overwrite");
			}
			
			boolean reload = true;
			if(zMessage.exists("reload")){
				reload = zMessage.getBoolean("reload");
			}

			//Hash it..
			//MiniData hash     = Crypto.getInstance().hashObject(data, 160);
			byte[] hashdata = Crypto.getInstance().hashData(data.getData(), 160);
			MiniData hash   = new MiniData(hashdata);
			
			String minidappid = hash.to0xString();
			InputHandler.getResponseJSON(zMessage).put("uid", minidappid);
			
			//This is the folder..
			File alldapps = Main.getMainHandler().getBackupManager().getMiniDAPPFolder();
			
			//And the actual folder...
			File dapp  = new File(alldapps,hash.to0xString());
			if(dapp.exists() && !overwrite){
				InputHandler.endResponse(zMessage, true, "MiniDAPP already installed..");
				return;
			}
			
			//Make the DAPP folder
			dapp.mkdirs();
			
			//Make a time stamp of the time of install..
			File ts = new File(dapp,"minimatimestamp");
			if(ts.exists()) {
				ts.delete();
			}
			FileOutputStream tsfos = new FileOutputStream(ts);
			DataOutputStream daos  = new DataOutputStream(tsfos);
			daos.writeLong(System.currentTimeMillis());
			daos.close();
			tsfos.close();
			
			//Now extract the contents to that folder..
			byte[] buffer = new byte[2048];
			ByteArrayInputStream bais = new ByteArrayInputStream(data.getData());
			BufferedInputStream bis = new BufferedInputStream(bais);
            ZipInputStream stream   = new ZipInputStream(bis);
	        ZipEntry entry          = null;
	        
	        //First find the minidapp.conf file..
	        String folder = "";
	        while ((entry = stream.getNextEntry()) != null) {
	        	//The file name..
	        	String name = entry.getName();
	        	
	        	//Is it the file
	        	if(name.endsWith("minidapp.conf")) {
	        		//OK - we have it.. what folder
	        		folder = name.substring(0, name.length()-13);	        		
	        		MinimaLogger.log("Found minidapp.conf @ "+name+" folder:"+folder);
	        		break;
	        	}
	        }
	        
	        //Close it up..
	        stream.close();
	        bis.close();
	        bais.close();
	        
	        //reset the zip stream..
	        bais 	= new ByteArrayInputStream(data.getData());
	        bis 	= new BufferedInputStream(bais);
            stream  = new ZipInputStream(bis);
	        entry   = null;
	        
	        //Cycle through all the files..
	        while ((entry = stream.getNextEntry()) != null) {
	        	//The file name..
	        	String name = entry.getName();

	        	//Strip folder from name..
	        	if(name.startsWith(folder)) {
	        		name = name.substring(folder.length());
	        	}else {
	        		MinimaLogger.log("WARNING : File outside of Main folder ["+folder+"] in MiniDAPP ["+filename+"] "+name);
	        	}
	        	
	        	//Where does this file go
	            File filePath = new File(dapp,name);
	
	            //Check the Parent
	            File parent = filePath.getParentFile();
	            if(!parent.exists()) {
	            	parent.mkdirs();
	            }
	            
	            //Do we need to make the directory
				if(entry.isDirectory()) {
					filePath.mkdirs();	
	            }else {
					//Delete if exists..
	            	if(filePath.exists()){
	            		filePath.delete();
	            	}
	            	
	            	//read it in and pump it out
		            FileOutputStream fos     = new FileOutputStream(filePath);
		            BufferedOutputStream bos = new BufferedOutputStream(fos, buffer.length);
		            
	                int len;
	                while ((len = stream.read(buffer)) > 0) {
	                    bos.write(buffer, 0, len);
	                }
	                
	                //Flush the system..
	                bos.flush();
	                
	                //And close..
	                bos.close();
	                fos.close();
	            }
	        }
	        
	        //And finally write the data..
	        File download = new File(dapp,filename);
	        MiniFile.writeDataToFile(download, data.getData());
	        
	        //It's done!
	        if(reload) {
				recalculateMiniDAPPS();
	        }
	        
			InputHandler.endResponse(zMessage, true, "MiniDAPP installed..");
			
			//Notify those listening..
			Main.getMainHandler().getConsensusHandler()
				.updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_DAPP_INSTALLED).addString("name", filename));
			
		}else if(zMessage.getMessageType().equals(DAPP_UNINSTALL)) {
			String minidapp = zMessage.getString("minidapp");
			InputHandler.getResponseJSON(zMessage).put("minidapp", minidapp);
			
			MinimaLogger.log("UNINSTALLING : "+minidapp);
			
			//Delete the DB
			if(SQLHandler.isMySQLEnabled()) {
				//What is the DB
				String db = SQLHandler.getMiniDappMySQLName(minidapp);
				
				//Create the DROP SQL
				String drop = "DROP DATABASE "+db;
				
				//Delete the MySQL DB
				SQL sqldel = new SQL(drop, minidapp);
				sqldel.run();
			}else {
				//Close the DB connection..
				
				
			}
			
			//UNINSTALL the DAPP
			File appfolder = new File(getMiniDAPPSFolder(),minidapp);
		
			if(!appfolder.exists()) {
				InputHandler.endResponse(zMessage, false, "MiniDAPP not found..");	
				return;
			}
			
			//Close the DB connections first..
			SQLHandler.CloseSQL();
			
			//Delete the app root..
			BackupManager.safeDelete(appfolder);
			
			//Recalculate the MINIDAPPS
			recalculateMiniDAPPS();
			
			InputHandler.endResponse(zMessage, true, "MiniDAPP uninstalled..");
			
		}else if(zMessage.getMessageType().equals(DAPP_DIRECTPOST)) {
			//Send a MinimaEvent Message to a specific minidapp
			String minidapp = zMessage.getString("minidapp");
			String message  = zMessage.getString("message");
			
			//Create a unique REPLY ID
			String replyid = MiniData.getRandomData(20).to0xString();
			
			//Check time..
			long timenow = System.currentTimeMillis();
			if(timenow - mLastReplyUsed > 20000) {
				//Clear the whole thing..
				mReplyMessage.clear();
			}
			mLastReplyUsed = timenow;
			
			//Put a link to this..
			mReplyMessage.put(replyid, zMessage);
			
			//Make a JSON
			JSONObject json = new JSONObject();
			json.put("action", "post");
			json.put("message", message);
			json.put("replyid", replyid);
			
			JSONObject wsmsg = new JSONObject();
			wsmsg.put("event","network");
			wsmsg.put("details",json);
			
			//Send to the backend
			sendToBackEND(minidapp,wsmsg);
			
			//And to the front end..
			Message msg = new Message(WebSocketManager.WEBSOCK_SEND);
			msg.addString("message", wsmsg.toString());
			msg.addString("minidappid", minidapp);
			mNetwork.getWebSocketManager().PostMessage(msg);
		
		}else if(zMessage.getMessageType().equals(DAPP_DIRECTREPLY)) {
			//Get the REPLY ID
			String replyid = zMessage.getString("replyid");
			
			//Get the message
			String message = zMessage.getString("message");
			
			//Get the Message..
			Message msg = mReplyMessage.remove(replyid);
			mLastReplyUsed = System.currentTimeMillis();
			
			//Do we have it..
			if(msg != null) {
				//Woo Hoo!
				InputHandler.getResponseJSON(msg).put("reply", message.toString());
				InputHandler.endResponse(msg, true, "Message posted");
			}
			
		}else if(zMessage.isMessageType(DAPP_MINIDAPP_POST)) {
			//What is the message..
			String minidapp = zMessage.getString("minidapp");
			JSONObject json = (JSONObject) zMessage.getObject("message");
			
			//First the Back End..
			sendToBackEND(minidapp,json);
			
			//Remove funny characters
			String characterFilter = "[^\\p{L}\\p{M}\\p{N}\\p{P}\\p{Z}\\p{Cf}\\p{Cs}\\s]";
			String JSONEvent = json.toString().replaceAll(characterFilter,"");
			
			Message msg = new Message(WebSocketManager.WEBSOCK_SEND);
			msg.addString("minidappid", minidapp);
			msg.addString("message", JSONEvent);
			mNetwork.getWebSocketManager().PostMessage(msg);
			
		}else if(zMessage.isMessageType(DAPP_MINIDAPP_POSTALL)) {
			//What is the message..
			JSONObject json = (JSONObject) zMessage.getObject("message");
			
			//First the Back End..
			sendToBackEND("",json);
			
			//Remove funny characters
			String characterFilter = "[^\\p{L}\\p{M}\\p{N}\\p{P}\\p{Z}\\p{Cf}\\p{Cs}\\s]";
			String JSONEvent = json.toString().replaceAll(characterFilter,"");
			
			Message msg = new Message(WebSocketManager.WEBSOCK_SENDTOALL);
			msg.addString("message", JSONEvent);
			mNetwork.getWebSocketManager().PostMessage(msg);
			
			//Post it to an URL..
			try {
				//Get the URL
				String url = mNetwork.getExternalURL();
				if(!url.equals("")) {
					String reply = RPCClient.sendPOST(url, JSONEvent, "application/json");
				}
			}catch(Exception exc) {
				MinimaLogger.log("ExternalURL error : "+exc.toString()+" "+json.toString());
			}
		}	
	}
	
	private void sendToBackEND(String zMiniDAPPID, JSONObject zJSON) {
		//Create the same EVent as on the Web
	    String JSONEvent = zJSON.toString();
	    
	    //Remove EMOJI and other weird symbols
		String characterFilter = "[^\\p{L}\\p{M}\\p{N}\\p{P}\\p{Z}\\p{Cf}\\p{Cs}\\s]";
		JSONEvent = JSONEvent.replaceAll(characterFilter,"");
	    
		if(zMiniDAPPID.equals("")){
			Enumeration<BackEndDAPP> bends = mBackends.elements();
			while(bends.hasMoreElements()) {
				//Get the next backend..
				BackEndDAPP bend = bends.nextElement();
				
				//Send it..
				bend.MinimaEvent(JSONEvent);
			}
		}else {
			BackEndDAPP bend = mBackends.get(zMiniDAPPID);
			if(bend != null) {
				//Send it..
				bend.MinimaEvent(JSONEvent);
			}
		}	
	}
	
	public File findFile(File zRootDirectory, String zFilename) {
		File[] subs = zRootDirectory.listFiles();
		if(subs != null) {
			for(File app : subs) {
				if(app.isDirectory()) {
					File found = findFile(app, zFilename);
					if(found!=null) {
						return found;
					}	
				}
				
				if(app.isFile()) {
					if(app.getName().equals(zFilename)) {
						return app;
					}
				}
			}
		}
		
		return null;
	}
}
