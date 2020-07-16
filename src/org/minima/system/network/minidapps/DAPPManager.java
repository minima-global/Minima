package org.minima.system.network.minidapps;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.minidapps.comms.CommsManager;
import org.minima.system.network.minidapps.minihub.hexdata.minimajs;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
import org.minima.utils.messages.Message;
import org.minima.utils.nanohttpd.protocols.http.NanoHTTPD;

public class DAPPManager extends SystemHandler {

	public static String DAPP_INSTALL   = "DAPP_INSTALL";
	public static String DAPP_UNINSTALL = "DAPP_UNINSTALL";
	public static String DAPP_UPDATE    = "DAPP_UPDATE";
	
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
	
	public DAPPManager(Main zMain) {
		super(zMain, "DAPPMAnager");
		
		//Need access to this
		mNetwork = getMainHandler().getNetworkHandler();
		
		//What is the current Host
		mOldHost  = mNetwork.getBaseHost();
		mBasePort = mNetwork.getBasePort();
				
		mCommsManager = new CommsManager(zMain);
		
		//Now create the Minima JS file..
	    recalculateMinimaJS();
	    
		//Calculate the current MiniDAPPS
		recalculateMiniDAPPS();
		
		mDAPPServer = new DAPPServer(mNetwork.getMiniDAPPServerPort(), this);
		try {
			mDAPPServer.start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
			MinimaLogger.log("MiniDAPP server started on por "+mNetwork.getMiniDAPPServerPort());
		} catch (IOException e) {
			MinimaLogger.log(e.toString());
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public CommsManager getCommsManager() {
		return mCommsManager;
	}

	
	public void recalculateMinimaJS() {
		//Now create the Minima JS file..
	    try {
			//Get the bytes..
	    	byte[] minima = minimajs.returnData();
		
	    	//create a string..
	    	String minstring = new String(minima, Charset.forName("UTF-8"));
	    	
	    	//What is the RPC address
	    	String rpcaddress = mOldHost+":"+mNetwork.getRPCPort();
	    	
	    	//Now replace the RPC connect address..
		    String editstring = minstring.replace("127.0.0.1:9002",rpcaddress);
	 
		    //What is the WebSocket address
	    	String wsaddress = mOldHost+":"+mNetwork.getWSPort();
	    	
		    //Replace the Web Socket Server IP..
		    editstring = editstring.replace("127.0.0.1:9003",wsaddress);
			
		    //Now convert to bytes..
		    mMINIMAJS = editstring.getBytes();
	    
		    //MinimaLogger.log(editstring);
		    
	    } catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public byte[] getMinimaJS() {
//		//Check if the Host has changed..
//		String host = getHostIP();
//		if(!host.equals(mHost)) {
//			MinimaLogger.log("MINIDAPP RPCHOST CHANGED from "+mHost+" to "+host);
//			mHost = host;
//			recalculateMinimaJS(mHost,mRPCPort);
//		}
		
		return mMINIMAJS;
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
	
	public JSONObject loadConfFile(File zConf) {
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
	        int firstfolder = approot.indexOf("/");
	        if(firstfolder != -1) {
	        	approot = approot.substring(0,firstfolder);
	        }
	        
	        ret.put("root", webroot);
	        ret.put("approot", approot);
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
	
	public JSONArray recalculateMiniDAPPS() {
		//Clear the OLD
		CURRENT_MINIDAPPS.clear();
		
		//This is the folder..
		File alldapps = getMainHandler().getBackupManager().getMiniDAPPFolder();
		
		//Store for later
		MINIDAPPS_FOLDER = alldapps.getAbsolutePath();
		
		//List it..
		File[] apps = alldapps.listFiles();
		
		//Each MiniDAPP gets it's OWN port..
		int miniport = 1;
		
		//Cycle through them..
		if(apps != null) {
			for(File app : apps) {
				//Open it up..
				File conf = new File(app,"minidapp.conf");
				
				//Does it exist..
				if(!conf.exists()) {
					//Could be 1 folder down..
					File[] subapps = app.listFiles();
		
					//Has to be the first file
					if(subapps != null) {
						for(File subapp : subapps) {
							//Ignore the SQL folder that we generate..
							if(subapp.isDirectory()) {
								conf = new File(subapp,"minidapp.conf");
								if(conf.exists()) {
									break;	
								}
							}
						}
					}
				}
				
				//Check it exists..
				if(conf.exists()) {
					//Load it..
					JSONObject confjson = loadConfFile(conf);
					
					///Give it a unique Port..
					confjson.put("port", miniport++);
					
					//Add it..
					CURRENT_MINIDAPPS.add(confjson);
				}
			}
		}
		
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
		if(zMessage.getMessageType().equals(DAPP_INSTALL)) {
			//Get the Data
			MiniData data = (MiniData) zMessage.getObject("minidapp");
			
			//Do we overwrite..
			boolean overwrite = false;
			if(zMessage.exists("overwrite")){
				overwrite = zMessage.getBoolean("overwrite");
			}

			//Hash it..
			MiniData hash = Crypto.getInstance().hashObject(data, 160);
			InputHandler.getResponseJSON(zMessage).put("UID", hash.to0xString());
			
			//This is the folder..
			File alldapps = getMainHandler().getBackupManager().getMiniDAPPFolder();
			
			//And the actual folder...
			File dapp  = new File(alldapps,hash.to0xString());
			if(dapp.exists() && !overwrite){
				InputHandler.endResponse(zMessage, true, "MiniDAPP ALLREADY installed..");
				return;
			}
			
			dapp.mkdirs();
			
			//Now extract the contents to that folder..
			byte[] buffer = new byte[2048];
			ByteArrayInputStream bais = new ByteArrayInputStream(data.getData());
			
			BufferedInputStream bis = new BufferedInputStream(bais);
            ZipInputStream stream   = new ZipInputStream(bis);
	        ZipEntry entry          = null;
	        
	        //Cycle through all the files..
	        while ((entry = stream.getNextEntry()) != null) {
	        	//Where does this file go
	            File filePath = new File(dapp,entry.getName());
	
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
	        
	        //It's done!
			recalculateMiniDAPPS();
		
			InputHandler.endResponse(zMessage, true, "MiniDAPP installed..");
			
		}else if(zMessage.getMessageType().equals(DAPP_UNINSTALL)) {
			
			
		}else if(zMessage.getMessageType().equals(DAPP_UPDATE)) {
			//Send a MinimaEvent Message to all the current Backend DAPPS
			
			
		
		}
		
	}
	
}
