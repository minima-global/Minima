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
import java.nio.charset.Charset;
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
import org.minima.system.network.minidapps.comms.CommsManager;
import org.minima.system.network.minidapps.minibackend.BackEndDAPP;
import org.minima.system.network.minidapps.minihub.hexdata.minimajs;
import org.minima.system.network.minidapps.websocket.WebSocketManager;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
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
	
	//The old HOST..
	String mOldHost = "";
	int mBasePort   = 0;
	
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
		
		mReplyMessage = new Hashtable<>();
		
		//What is the current Host
		mOldHost  = mNetwork.getBaseHost();
		mBasePort = mNetwork.getBasePort();
		
		//Init the System
		PostMessage(DAPP_INIT);
	}
	
	public CommsManager getCommsManager() {
		return mCommsManager;
	}
	
	public void stop() {
		mDAPPServer.stop();
		mCommsManager.shutdown();
		
		stopMessageProcessor();
	}
	
	public JSONArray getMiniDAPPS() {
		return CURRENT_MINIDAPPS;
	}
	
	public String getMiniDAPPSFolder() {
		return MINIDAPPS_FOLDER;
	}
	
	private JSONObject loadConfFile(File zConf) {
		JSONObject ret = new JSONObject();
		
		try {
			StringBuilder tot = new StringBuilder();
			
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
	        
	        //And add the root folder..
	        String root = zConf.getParent();
	        int start = root.indexOf("/minidapps/");
	        String webroot = root.substring(start);
	        String approot = root.substring(start+11);
	        
	        ret.put("uid", approot);
	        ret.put("root", webroot);
	        ret.put("web", "http://"+mNetwork.getBaseHost()+":"+mNetwork.getMiniDAPPServerPort()+webroot);
	        
	        bis.close();
	        fis.close();
	        
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ParseException e) {
			e.printStackTrace();
		}
		
		return ret;
	}
	
	private JSONArray recalculateMiniDAPPS() {
		//Clear the OLD
		CURRENT_MINIDAPPS.clear();
		
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
				File conf    = new File(app,"minidapp.conf");
				File backend = new File(app,"service.js");
				
				//Check it exists..
				if(conf.exists()) {
					//Load it..
					JSONObject confjson = loadConfFile(conf);
					
					//Add the timestamp..
					confjson.put("installed", timestamp);
					confjson.put("download", download);
					
					//Is there a Back end
					if(backend.exists()) {
						//Load it..
						try {
							//Load the JS file..
							String backjs = new String(MiniFile.readCompleteFile(backend),"UTF-8");
						
							//Create a BackEnd APP..
							BackEndDAPP bedapp = new BackEndDAPP(backjs, minidappid);
							
							//Add to the List..
							mBackends.put(minidappid, bedapp);
						
							MinimaLogger.log("BackEndJS create for "+minidappid);
							
						} catch (Exception e) {
							MinimaLogger.log("Error loading service.js for "+backend.getAbsolutePath()+" "+e);
						} 
					}
					
					//Add it..
					CURRENT_MINIDAPPS.add(confjson);
				}
			}
		}
		
		//Post a CONNECTED message to all the BackEnds.. 
		JSONObject wsmsg = new JSONObject();
		wsmsg.put("event","connected");
		wsmsg.put("details","success");
		sendToBackEND("", wsmsg);
		
		//Order the List.. By Name..
		Collections.sort(CURRENT_MINIDAPPS, new Comparator<JSONObject>() {
			@Override
			public int compare(JSONObject o1, JSONObject o2) {
				try {
					//In case the name is missing..
					String name1 = (String) o1.get("name");
					String name2 = (String) o2.get("name");	
					return name1.compareTo(name2);
					
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
				MinimaLogger.log("MiniDAPP server started on por "+mNetwork.getMiniDAPPServerPort());
				
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
			MiniData hash     = Crypto.getInstance().hashObject(data, 160);
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
	        
	        //Cycle through all the files..
	        boolean first = true;
	        String folder = "";
	        while ((entry = stream.getNextEntry()) != null) {
	        	//The file name..
	        	String name = entry.getName();
	        	
	        	//The Name..
	        	if(first) {
	        		first = false;
	        		if(entry.isDirectory()) {
						//OK - strip this from all future files..
		        		folder = name;
		            }	
	        	}
	        	
	        	//Strip folder from name..
	        	name = name.substring(folder.length());
	        	
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
			
			//UNINSTALL the DAPP
			File appfolder = new File(getMiniDAPPSFolder(),minidapp);
		
			if(!appfolder.exists()) {
				InputHandler.endResponse(zMessage, false, "MiniDAPP not found..");	
				return;
			}
			
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
			
			Message msg = new Message(WebSocketManager.WEBSOCK_SEND);
			msg.addString("minidappid", minidapp);
			msg.addString("message", json.toString());
			mNetwork.getWebSocketManager().PostMessage(msg);
			
		}else if(zMessage.isMessageType(DAPP_MINIDAPP_POSTALL)) {
			//What is the message..
			JSONObject json = (JSONObject) zMessage.getObject("message");
			
			//First the Back End..
			sendToBackEND("",json);
			
			Message msg = new Message(WebSocketManager.WEBSOCK_SENDTOALL);
			msg.addString("message", json.toString());
			mNetwork.getWebSocketManager().PostMessage(msg);
		}
		
	}
	
	private void sendToBackEND(String zMiniDAPPID, JSONObject zJSON) {
		//Create the same EVent as on the Web
	    String JSONEvent = zJSON.toString();
	    
	    //MinimaLogger.log("SEND TO BACKEND : "+JSONEvent);
	    
		if(zMiniDAPPID.equals("")){
			Enumeration<BackEndDAPP> bends = mBackends.elements();
			while(bends.hasMoreElements()) {
				BackEndDAPP bend = bends.nextElement();
				bend.MinimaEvent(JSONEvent);
			}
		}else {
			BackEndDAPP bend = mBackends.get(zMiniDAPPID);
			if(bend != null) {
				bend.MinimaEvent(JSONEvent);
			}
		}	
	}
}
