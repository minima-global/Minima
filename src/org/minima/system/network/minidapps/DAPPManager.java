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
import java.util.Collections;
import java.util.Comparator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
import org.minima.utils.messages.Message;

public class DAPPManager extends SystemHandler {

	public static String DAPP_INSTALL = "DAPP_INSTALL";
	
	JSONArray CURRENT_MINIDAPPS = new JSONArray();
	String MINIDAPPS_FOLDER     = "";
	
	DAPPServer mDAPPServer;
	
	public DAPPManager(Main zMain, int zPort) {
		super(zMain, "DAPPMAnager");
		
		//Calculate the current MiniDAPPS
		recalculateMiniDAPPS();
		
		///Start the DAPP server
		mDAPPServer = new DAPPServer(zPort,this);
		Thread tt = new Thread(mDAPPServer);
		tt.start();
	}
	
	public void stop() {
		mDAPPServer.stop();
		
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
		String root = getMainHandler().getBackupManager().getRootFolder();
		
		//Create the new Folder...
		File alldapps = new File(root,"minidapps");
		if(!alldapps.exists()) {
			alldapps.mkdirs();
		}
		
		//Store for later
		MINIDAPPS_FOLDER = alldapps.getAbsolutePath();
		
		//List it..
		File[] apps = alldapps.listFiles();
		
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
						if(subapps[0].isDirectory()) {
							conf = new File(subapps[0],"minidapp.conf");
						}
					}
				}
				
				//Check it exists..
				if(conf.exists()) {
					//Load it..
					JSONObject confjson = loadConfFile(conf);
					
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
			
			//Hash it..
			MiniData hash = Crypto.getInstance().hashObject(data, 160);
			
			//This is the folder..
			String root = getMainHandler().getBackupManager().getRootFolder();
			
			//Create the new Folder...
			File alldapps = new File(root,"minidapps");
			
			//And the actual folder...
			File dapp  = new File(alldapps,hash.to0xString());
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
