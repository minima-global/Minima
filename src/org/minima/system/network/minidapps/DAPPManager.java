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
import org.minima.system.network.minidapps.hexdata.minimajs;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
import org.minima.utils.messages.Message;
import org.minima.utils.nanohttpd.protocols.http.NanoHTTPD;

public class DAPPManager extends SystemHandler {

	public static String DAPP_INSTALL = "DAPP_INSTALL";
	
	JSONArray CURRENT_MINIDAPPS = new JSONArray();
	String MINIDAPPS_FOLDER     = "";
	
	
	ArrayList<NanoDAPPServer> mDAPPServers = new ArrayList<>();
	
	NanoDAPPServer mNanoDAPPServer;
	
	//The Edited minima.js file..
	byte[] mMINIMAJS = new byte[0];
	
	//HOST  - this will be inserted into the minima.js file
	String mHost = "";	
	boolean mHardSet = false;
	int mRPCPort;
	
	public DAPPManager(Main zMain, String zHost, int zPort, int zRPCPort) {
		super(zMain, "DAPPMAnager");
		
		//Correct HOST from RPC server
		mRPCPort = zRPCPort;
		
		mHost = "127.0.0.1";
		if(!zHost.equals("")) {
			mHardSet = true;
			mHost = zHost;
		}
		
		//Calculate the HOST
		getHostIP();
	
	    //Now create the Minima JS file..
	    recalculateMinimaJS(mHost,mRPCPort);
	    
		//Calculate the current MiniDAPPS
		recalculateMiniDAPPS();
		
		mNanoDAPPServer = new NanoDAPPServer(zPort, this);
		try {
			mNanoDAPPServer.start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
			MinimaLogger.log("MiniDAPP server started on "+mHost+":"+zPort);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public String getHostIP() {
		if(mHardSet) {
			return mHost;
		}
		
		String host = "127.0.0.1";
		try {
			boolean found = false;
		    Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
	        while (!found && interfaces.hasMoreElements()) {
	            NetworkInterface iface = interfaces.nextElement();
	            // filters out 127.0.0.1 and inactive interfaces
	            if (iface.isLoopback() || !iface.isUp())
	                continue;

	            Enumeration<InetAddress> addresses = iface.getInetAddresses();
	            while(!found && addresses.hasMoreElements()) {
	                InetAddress addr = addresses.nextElement();
	                String ip   = addr.getHostAddress();
	                String name = iface.getDisplayName();
	                
	                //Only get the IPv4
	                if(!ip.contains(":")) {
	                	host = ip;
	                	
	                	//If you're on WiFi..
	                	if(name.startsWith("wl")) {
	                		found = true;
	                		break;
	                	}
	                }
	            }
	        }
	    } catch (SocketException e) {
	        System.out.println("DAPPMAnager getHostIP : "+e);
	    }
		
		return host;
	}
	
	public void recalculateMinimaJS(String zHost, int zPort) {
		//Now create the Minima JS file..
	    try {
			//Get the bytes..
	    	byte[] minima = minimajs.returnData();
		
	    	//create a string..
	    	String minstring = new String(minima, Charset.forName("UTF-8"));
	    
	    	//Now replace the center string..
		    String editstring = minstring.replace("var MINIMA_MINIDAPP_HOST = \"127.0.0.1:8999\";", 
										    	  "var MINIMA_MINIDAPP_HOST = \""+zHost+":"+zPort+"\";");
	 
		    //Replace the Websocket Server IP..
		    editstring = editstring.replace("ws://127.0.0.1:20999", "ws://"+zHost+":20999");
			
		    //It's a MiniDAPP
		    editstring = editstring.replace("var MINIMA_IS_MINIDAPP = false;", 
		    		                        "var MINIMA_IS_MINIDAPP = true;");
			
		    //Now convert to bytes..
		    mMINIMAJS = editstring.getBytes();
	    
	    } catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//Use the RPC server for now..
	public String getCurrentHost() {
		return mHost;
	}
	
	public byte[] getMinimaJS() {
		//Check if the Host has changed..
		String host = getHostIP();
		if(!host.equals(mHost)) {
			MinimaLogger.log("MINIDAPP RPCHOST CHANGED from "+mHost+" to "+host);
			mHost = host;
			recalculateMinimaJS(mHost,mRPCPort);
		}
		
		return mMINIMAJS;
	}
	
	public void stop() {
		mNanoDAPPServer.stop();
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
			boolean overwrite = true;
			if(zMessage.exists("overwrite")){
				overwrite = zMessage.getBoolean("overwrite");
			}

			//Hash it..
			MiniData hash = Crypto.getInstance().hashObject(data, 160);
			
			//This is the folder..
			File alldapps = getMainHandler().getBackupManager().getMiniDAPPFolder();
			
			//And the actual folder...
			File dapp  = new File(alldapps,hash.to0xString());
			if(dapp.exists() && !overwrite){
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
		}
		
	}
	
}
