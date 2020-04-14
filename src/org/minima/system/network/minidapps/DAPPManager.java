package org.minima.system.network.minidapps;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
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
	        ret.put("root", "minidapps/"+zConf.getParentFile().getName());
	        
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
		CURRENT_MINIDAPPS.clear();
		for(File app : apps) {
			//Open it up..
			File conf = new File(app,"minidapp.conf");
			
			if(conf.exists()) {
				//Load it..
				JSONObject confjson = loadConfFile(conf);
				
				//Add it..
				CURRENT_MINIDAPPS.add(confjson);
			}
		}
		
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
			
			//Now extract the contents to that folder..
			byte[] buffer = new byte[2048];
			ByteArrayInputStream bais = new ByteArrayInputStream(data.getData());
			
			BufferedInputStream bis = new BufferedInputStream(bais);
            ZipInputStream stream   = new ZipInputStream(bis);
            
	        ZipEntry entry;
	        
	        while ((entry = stream.getNextEntry()) != null) {
	        	//Where does this file go
	            File filePath = new File(dapp,entry.getName());
	
	            //Check the parent folder exists
	            File parent = filePath.getParentFile();
	            if(!parent.exists()) {
	            	parent.mkdirs();
	            }
	            
	            FileOutputStream fos     = new FileOutputStream(filePath);
	            BufferedOutputStream bos = new BufferedOutputStream(fos, buffer.length);
	            
                int len;
                while ((len = stream.read(buffer)) > 0) {
                    bos.write(buffer, 0, len);
                }
	        }
	        
	        //It's done!
			recalculateMiniDAPPS();
		}
		
	}
	
}
