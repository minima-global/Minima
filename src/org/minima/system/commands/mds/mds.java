package org.minima.system.commands.mds;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ZipExtractor;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class mds extends Command {

	public mds() {
		super("mds","(action:list|install|uninstall) (file:) (uid:) - MiniDAPP System management");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		String action = getParam("action","list");
		
		MDSDB db = MinimaDB.getDB().getMDSDB();
		
		if(action.equals("list")) {
			
			//List the current MDS apps..
			ArrayList<MiniDAPP> dapps = db.getAllMiniDAPPs();
			
			JSONArray arr = new JSONArray();
			for(MiniDAPP md : dapps) {
				arr.add(md.toJSON());
			}

			ret.put("minidapps", arr);
		
		}else if(action.equals("install")) {
		
			String file = getParam("file");
			
			//Now start
			FileInputStream fis = new FileInputStream(file);
			
			//Where is it going..
			String rand = MiniData.getRandomData(16).to0xString();
			
			//The file where the package is extracted..
			File dest 	= new File( Main.getInstance().getMDSManager().getWebFolder() , rand);
			if(dest.exists()) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
			dest.mkdirs();
			
			//Send it to the extractor..
			ZipExtractor.unzip(fis, dest);
			fis.close();
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(new File(dest,"dapp.conf"))); 	
			
			//Now create the JSON..
			JSONObject json = (JSONObject) new JSONParser().parse(data.toString());
			
			//Create the MiniDAPP
			MiniDAPP md = new MiniDAPP(rand, json.getString("name"), 
					json.getString("icon"), json.getString("version"), json.getString("description"));
			
			//Now add to the DB
			db.insertMiniDAPP(md);
			
			ret.put("installed", md.toJSON());
			
		}else if(action.equals("uninstall")) {

			String uid = getParam("uid");
			
			//Start deleting..
			File dest 	= Main.getInstance().getMDSManager().getWebFolder();
			MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			
			//And from the DB
			db.deleteMiniDAPP(uid);
			
			ret.put("uninstalled", uid);
			
		}else if(action.equals("reload")) {
			
			JSONArray arr = new JSONArray();
			
			File root = Main.getInstance().getMDSManager().getWebFolder();
			File[] files = root.listFiles();
			if(files != null) {
			
				for(File dapp : files) {
				
					//Get the File name
					String uid = dapp.getName();
					db.deleteMiniDAPP(uid);
					
					//Load the conf file..
					MiniString data = new MiniString(MiniFile.readCompleteFile(new File(dapp,"dapp.conf"))); 	
					
					//Now create the JSON..
					JSONObject json = (JSONObject) new JSONParser().parse(data.toString());
					
					//Create the MiniDAPP
					MiniDAPP md = new MiniDAPP(uid, json.getString("name"), 
							json.getString("icon"), json.getString("version"), json.getString("description"));
					
					//Now add to the DB
					db.insertMiniDAPP(md);
					
					//Add to the final array
					arr.add(md.toJSON());
				}
			}
			
			ret.put("minidapps", arr);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new mds();
	}

}
