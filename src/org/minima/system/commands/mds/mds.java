package org.minima.system.commands.mds;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.mds.MDSManager;
import org.minima.system.mds.handler.CMDcommand;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ZipExtractor;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;

public class mds extends Command {

	public mds() {
		super("mds","(action:list|install|update|uninstall|pending|accept|deny|permission) (file:) (uid:) (trust:read|write)- MiniDAPP System management");
	}
	
	
	@Override
	public String getFullHelp() {
		return "\nmds\n"
				+ "\n"
				+ "MiniDAPP System management.\n"
				+ "\n"
				+ "Install, update or uninstall MiniDapps and set their permissions to READ/WRITE. Default permission is READ.\n"
				+ "\n"
				+ "DO NOT give WRITE permissions to MiniDapps you do not trust!"
				+ "\n"
				+ "Accept/deny pending commands from MiniDapps with READ permissions.\n"
				+ "\n"
				+ "The Public MDS (if enabled) is accessible from your https://127.0.0.1:MDS_PORT/publicmds/ \n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    list : List your installed MiniDapps. Default parameter.\n"
				+ "    install : Install a new MiniDapp and optionally set its permission. Must specify 'file'.\n"
				+ "    update : Update and replace an existing MiniDapp. Must specify MiniDapp 'uid' and 'file' of new MiniDapp.\n"
				+ "    uninstall : Uninstall a MiniDapp. Must specify MiniDapp 'uid'.\n"
				+ "    download : Download the MiniDapp to your BASE folder.\n"
				+ "    pending : List all pending commands waiting to be accepted or denied.\n"
				+ "    accept : Accept a pending command. Must specify 'uid' of the pending command.\n"
				+ "    deny : Deny a pending command. Must specify 'uid' of the pending command.\n"
				+ "    permission : Set permission for a MiniDapp to READ or WRITE. Must specify existing MiniDapp 'uid' and 'trust'.\n"
				+ "    publicmds : Enable or disable the PUblic MDS. \n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    The file name of the MiniDapp to install. Can either be in the Minima folder or specify the file path.\n"
				+ "\n"
				+ "uid (optional)\n"
				+ "    The uid of the MiniDapp to update, uninstall.\n"
				+ "\n"
				+ "trust: (optional)\n"
				+ "    The ip:port to send a message to. Use with 'action:send'.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "mds\n"
				+ "\n"
				+ "mds action:list\n"
				+ "\n"
				+ "mds action:install file:wallet_1.0.mds.zip \n"
				+ "\n"
				+ "mds action:install file:/Users/MyUser/Downloads/wallet_1.0.mds.zip trust:write\n"
				+ "\n"
				+ "mds action:update uid:0xABA3.. file:wallet_2.0.mds.zip \n"
				+ "\n"
				+ "mds action:uninstall uid:0xABA3..\n"
				+ "\n"
				+ "mds action:download uid:0xABA3..\n"
				+ "\n"
				+ "mds action:download uid:0xABA3.. locationonly:true\n"
				+ "\n"
				+ "mds action:download uid:0xABA3.. folder:Downloads\n"
				+ "\n"
				+ "mds action:pending\n"
				+ "\n"
				+ "mds action:accept uid:0xCDF6..\n"
				+ "\n"
				+ "mds action:deny uid:0xCDF6..\n"
				+ "\n"
				+ "mds action:publicmds enable:true\n"
				+ "\n"
				+ "mds action:permission uid:0xABA3.. trust:write\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","file","folder",
				"uid","trust","enable","locationonly"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		String action = getParam("action","list");
		
		MDSDB db = MinimaDB.getDB().getMDSDB();
		
		if(action.equals("list")) {
			
			MDSManager mdsman = Main.getInstance().getMDSManager();
			
			//List the current MDS apps..
			ArrayList<MiniDAPP> dapps = db.getAllMiniDAPPs();
			
			JSONArray arr = new JSONArray();
			for(MiniDAPP md : dapps) {
				
				//Get the MiniDAPP JSON
				JSONObject jmds = md.toJSON(); 
				
				//Get the Session ID
				String sessionid = mdsman.convertMiniDAPPID(md.getUID());
				jmds.put("sessionid", sessionid);
				
				arr.add(jmds);
			}

			JSONObject mds = new JSONObject();
			mds.put("enabled", GeneralParams.MDS_ENABLED);
			
			mds.put("connect", "https://"+GeneralParams.MINIMA_HOST+":"+GeneralParams.MDSFILE_PORT);
			mds.put("password", Main.getInstance().getMDSManager().getMiniHUBPasword());
			
			mds.put("publicmds", MinimaDB.getDB().getUserDB().getPublicMDS());
			mds.put("publicmdsuid", Main.getInstance().getMDSManager().getPublicMiniDAPPSessionID());
			mds.put("untrustedmdsuid", Main.getInstance().getMDSManager().getUntrustedMiniDAPPSessionID());
			
			mds.put("minidapps", arr);
			
			ret.put("response", mds);
		
		}else if(action.equals("pending")) {
			
			//Get all the pending commands..
			ArrayList<PendingCommand> allpending = Main.getInstance().getMDSManager().getAllPending(); 
			
			//Make into JSONArray
			JSONArray pend = new JSONArray();
			for(PendingCommand pending : allpending) {
				pend.add(pending.toJSON());
			}
			
			JSONObject mds = new JSONObject();
			mds.put("pending", pend);
		
			ret.put("response", mds);
		
		}else if(action.equals("accept")) {
			
			//Get the uid
			String uid = getParam("uid");
			
			//Get the pending command
			PendingCommand pending = Main.getInstance().getMDSManager().getPendingCommand(uid);
			String minidappid = "";
			if(pending!=null) {
				
				//Get the MiniDAPP  ID
				minidappid = pending.getMiniDAPP().getString("uid",""); 
				
				//RUN it.. as normal 0x00 - so is accepted
				CMDcommand cmd 	= new CMDcommand("0x00", pending.getCommand());
				String result 	= cmd.runCommand();
				
				JSONObject jsonres = new JSONObject();
				boolean status = false;
				if(result.startsWith("{")) {
					JSONObject response = (JSONObject) new JSONParser().parse(result);
					status = (boolean) response.get("status");
					
					ret.put("response", response);
				
					jsonres = response;
					
				}else if(result.startsWith("[")) {
					ret.put("response", (JSONArray) new JSONParser().parse(result));
				
				}else {
					ret.put("response", result);
				}
			
				//Remove it from the list
				Main.getInstance().getMDSManager().removePending(uid);
				
				//Post a message to the MiniDAPP
				sendPendingResult(minidappid, uid, true, status, jsonres);
				
			}else {
				throw new CommandException("Pending UID not found : "+uid);
			}
			
		}else if(action.equals("deny")) {
			
			//Get the uid
			String uid = getParam("uid");
			
			PendingCommand pending = Main.getInstance().getMDSManager().getPendingCommand(uid);
			String minidappid = "";
			if(pending!=null) {
				
				//Get the MiniDAPP  ID
				minidappid = pending.getMiniDAPP().getString("uid",""); 
			
				//Remove
				Main.getInstance().getMDSManager().removePending(uid);
				
			}else {
				throw new CommandException("Pending UID not found : "+uid);
			}
			
			ret.put("response", "Pending action removed : "+uid);
			
			//Post a message to the MiniDAPP
			sendPendingResult(minidappid, uid, false, false, new JSONObject());
			
		}else if(action.equals("install")) {
		
			String file = getParam("file");
			
			File minidapp = MiniFile.createBaseFile(file);
			if(!minidapp.exists()) {
				throw new CommandException("MiniDAPP not found.. : "+file);
			}
			
			//Now start
			FileInputStream fis = new FileInputStream(minidapp);
			
			//Where is it going..
			String rand = MiniData.getRandomData(32).to0xString();
			
			//The file where the package is extracted..
			File dest 	= new File( Main.getInstance().getMDSManager().getWebFolder() , rand);
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
				
				//Delete the install
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);	
				
				throw new CommandException("No dapp.conf file found");
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//ALWAYS starts with only READ Permission
			String trust = getParam("trust", "read");
			jsonconf.put("permission", trust);
			
			//Create the MiniDAPP
			MiniDAPP md = new MiniDAPP(rand, jsonconf);
			
			//Now add to the DB
			db.insertMiniDAPP(md);
			
			//Remove from the deleted list
			MinimaDB.getDB().getUserDB().removeUninstalledMiniDAPP(md.getName());
			MinimaDB.getDB().saveUserDB();
			
			//Now copy the minidapp itself..so you have a copy..
			File copyfolder = Main.getInstance().getMDSManager().getMiniDAPPCopyDappFolder(md.getUID());
			MiniFile.deleteFileOrFolder(copyfolder.getAbsolutePath(), copyfolder);
			copyfolder.mkdirs();
			File minisharefile 	= Main.getInstance().getMDSManager().getMiniDAPPShareFile(md);
			
			try {
				MiniFile.copyFile(minidapp, minisharefile);
			}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
			
			//All done..
			JSONObject mds = new JSONObject();
			mds.put("installed", md.toJSON());
			ret.put("response", mds);
			
			//There has been a change
			Message installed = new Message(MDSManager.MDS_MINIDAPPS_INSTALLED);
			installed.addObject("minidapp", md);
			Main.getInstance().getMDSManager().PostMessage(installed);
		
		}else if(action.equals("download")) {
			
			String uid = getParam("uid");
			if(!uid.startsWith("0x")) {
				throw new CommandException("Invalid UID for MiniDAPP");
			}
			
			//Get the Minidapp..
			File minisharefile 	= Main.getInstance().getMDSManager().getMiniDAPPShareFile(uid);
			if(!minisharefile.exists()) {
				throw new CommandException("Original MiniDAPP file does not exist "+minisharefile.getAbsolutePath());
			}
			
			//Do we just want the location
			boolean locationonly = getBooleanParam("locationonly", false);
			if(locationonly) {
				JSONObject mds = new JSONObject();
				mds.put("uid", uid);
				mds.put("original", minisharefile.getAbsolutePath());
				ret.put("response", mds);
				return ret;
			}
			
			File copyto 	= null;
			String folder	= getParam("folder", "");
			
			if(folder.equals("")) {
				
				//Where to place it..
				copyto = MiniFile.createBaseFile(minisharefile.getName());
				
			}else{
				
				//No back allowed
				folder.replace("..", "");
				
				//User folder is base folder
				File userFolder = new File(System.getProperty("user.home"),folder);
				copyto 			= new File(userFolder,minisharefile.getName());
			}
			
			//Log it.. 
			MinimaLogger.log("MDS Share Dapp : "+minisharefile.getAbsolutePath()+" to "+copyto.getAbsolutePath());
			MinimaLogger.log("PArent : "+copyto.getParentFile());
			
			//Check Parents Exist
			if(!copyto.getParentFile().exists()) {
				copyto.getParentFile().mkdirs();
			}
			
			//Now download..
			MiniFile.copyFile(minisharefile, copyto);
			
			//All done..
			JSONObject mds = new JSONObject();
			mds.put("uid", uid);
			mds.put("original", minisharefile.getAbsolutePath());
			mds.put("copy", copyto.getAbsolutePath());
			ret.put("response", mds);
			
		}else if(action.equals("uninstall")) {

			String uid = getParam("uid");
			if(!uid.startsWith("0x")) {
				throw new CommandException("Invalid UID for MiniDAPP");
			}
			
			//Make sure not the MiniHUB..
			String minihub = MinimaDB.getDB().getUserDB().getDefaultMiniHUB();
			if(uid.equals(minihub)) {
				throw new CommandException("Cannot delete the MiniHUB");
			}
			
			//Add to uninstalled..
			MiniDAPP md = Main.getInstance().getMDSManager().getMiniDAPP(uid);
			if(md == null) {
				throw new CommandException("MiniDAPP not found");
			}
			
			MinimaDB.getDB().getUserDB().addUninstalledMiniDAPP(md.getName());
			MinimaDB.getDB().saveUserDB();
			
			//Delete from the DB
			db.deleteMiniDAPP(uid);
			
			// Delete web..
			String mdsroot 	= Main.getInstance().getMDSManager().getRootMDSFolder().getAbsolutePath();
			File dest 		= Main.getInstance().getMDSManager().getWebFolder();
			File minidapp 	= new File(dest,uid);
			if(minidapp.exists()) {
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);
			}
			
			//Delete Data folder
			Main.getInstance().getMDSManager().shutdownSQL(uid);
			File dbfolder1 = Main.getInstance().getMDSManager().getMiniDAPPDataFolder(uid);
			if(dbfolder1.exists()) {
				MiniFile.deleteFileOrFolder(mdsroot, dbfolder1);
			}
			
			JSONObject mds = new JSONObject();
			mds.put("uninstalled", uid);
			ret.put("response", mds);
			
			//There has been a change
			Message uninstall = new Message(MDSManager.MDS_MINIDAPPS_UNINSTALLED);
			uninstall.addString("uid", uid);
			Main.getInstance().getMDSManager().PostMessage(uninstall);
			
		}else if(action.equals("update")) {
			
			String uid = getParam("uid");
			if(!uid.startsWith("0x")) {
				throw new CommandException("Invalid UID for MiniDAPP");
			}
			
			String file = getParam("file");
			File minifile = MiniFile.createBaseFile(file);
			if(!minifile.exists()) {
				throw new CommandException("MiniDAPP not found.. : "+file);
			}
			
			//Get the MiniDAPP
			MiniDAPP oldmd = db.getMiniDAPP(uid);
			if(oldmd == null) {
				throw new CommandException("MiniDAPP not found.. : "+uid);
			}
			
			//Get the Conf..
			JSONObject miniconf = oldmd.getConfData();
			
			//The MiniDAPP
			FileInputStream fis = new FileInputStream(minifile);
			
			//Delete ONLY the old WEB files
			String mdsroot 	= Main.getInstance().getMDSManager().getRootMDSFolder().getAbsolutePath();
			File minidapp 	= new File(Main.getInstance().getMDSManager().getWebFolder(),uid);
			if(minidapp.exists()) {
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);
			}
			
			//Extract the new files.. make sure exists
			minidapp.mkdirs();
			
			//Send it to the extractor..
			ZipExtractor.unzip(fis, minidapp);
			fis.close();
			
			//Is there a conf file..
			File conf = new File(minidapp,"dapp.conf");
			if(!conf.exists()) {
				
				//Delete the install
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);	
				
				throw new CommandException("No dapp.conf file found");
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//Copy the trust
			String trust = miniconf.getString("permission", "read");
			jsonconf.put("permission", trust);
			
			//Delete the old..
			db.deleteMiniDAPP(uid);
			
			//There has been a change
			Message uninstall = new Message(MDSManager.MDS_MINIDAPPS_UNINSTALLED);
			uninstall.addString("uid", uid);
			Main.getInstance().getMDSManager().PostMessage(uninstall);
			
			//The NEW miniDAPP
			MiniDAPP newmd = new MiniDAPP(uid, jsonconf);
			
			//Now add to the DB
			db.insertMiniDAPP(newmd);
			
			//Now copy the minidapp itself..so you have a copy..
			File copyfolder = Main.getInstance().getMDSManager().getMiniDAPPCopyDappFolder(newmd.getUID());
			MiniFile.deleteFileOrFolder(copyfolder.getAbsolutePath(), copyfolder);
			copyfolder.mkdirs();
			File minisharefile 	= Main.getInstance().getMDSManager().getMiniDAPPShareFile(newmd);
			
			try {
				MiniFile.copyFile(minifile, minisharefile);
			}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
			
			//There has been a change
			Message installed = new Message(MDSManager.MDS_MINIDAPPS_INSTALLED);
			installed.addObject("minidapp", newmd);
			Main.getInstance().getMDSManager().PostMessage(installed);
			
			JSONObject mds = new JSONObject();
			mds.put("updated", newmd.toJSON());
			ret.put("response", mds);
			
		}else if(action.equals("permission")) {
			
			String uid 		= getParam("uid");
			String trust 	= getParam("trust");
			
			//Check not Public or Untrusted
			if(uid.equals(Main.getInstance().getMDSManager().getPublicMiniDAPPID())) {
				throw new CommandException("Cannot set WRITE permission for Public MiniDAPP");
			
			}else if(uid.equals(Main.getInstance().getMDSManager().getUntrustedMiniDAPPID())) {
				throw new CommandException("Cannot set WRITE permission for Untrusted MiniDAPP");
			} 
			
			//Get the MIninDAPP..
			MiniDAPP md = db.getMiniDAPP(uid);
			if(md == null) {
				throw new CommandException("MiniDAPP not found : "+uid);
			}
			
			//Make sure not the MiniHUB..
			String minihub = MinimaDB.getDB().getUserDB().getDefaultMiniHUB();
			if(uid.equals(minihub)) {
				throw new CommandException("Cannot change permissions for the MiniHUB");
			}
			
			//Now update the TRUST level..
			if(trust.equals("write")) {
				md.setPermission("write");
			
			}else if(trust.equals("read")) {
				md.setPermission("read");
				
			}else {
				throw new CommandException("Invalid trust setting - must be read/write : "+trust);
			}
			
			//Now update.. delete the old / insert the new..
			db.deleteMiniDAPP(uid);
			
			//And insert..
			db.insertMiniDAPP(md);
			
			ret.put("response", md.toJSON());
			
		}else if(action.equals("reload")) {
			
			JSONArray arr = new JSONArray();
			
			File root = Main.getInstance().getMDSManager().getWebFolder();
			File[] files = root.listFiles();
			if(files != null) {
			
				for(File dapp : files) {
				
					//Get the File name
					String uid = dapp.getName();
					
					//Get the MiniDAPP
					MiniDAPP mdorig = db.getMiniDAPP(uid);
					if(mdorig != null) {
						String perm = mdorig.getPermission();
						
						db.deleteMiniDAPP(uid);
						
						//Load the conf file..
						MiniString data = new MiniString(MiniFile.readCompleteFile(new File(dapp,"dapp.conf"))); 	
						
						//Now create the JSON..
						JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
						jsonconf.put("permission", perm);
						
						//Create the MiniDAPP
						MiniDAPP md = new MiniDAPP(uid, jsonconf);
						
						//Now add to the DB
						db.insertMiniDAPP(md);
						
						//Add to the final array
						arr.add(md.toJSON());
					}else {
						
						MinimaLogger.log("EMPTY MiniDAPP folder deleted : "+dapp.getAbsolutePath());
						
						//Delete it..
						MiniFile.deleteFileOrFolder(dapp.getAbsolutePath(), dapp);
					}
				}
			}
			
			JSONObject mds = new JSONObject();
			mds.put("minidapps", arr);
			ret.put("response", mds);
			
			//There has been a change
			Main.getInstance().getMDSManager().PostMessage(MDSManager.MDS_MINIDAPPS_RESETALL);
		
		}else if(action.equals("publicmds")) {
			
			boolean enable = getBooleanParam("enable");
			
			MinimaDB.getDB().getUserDB().setPublicMDS(enable);
			
			JSONObject mds = new JSONObject();
			mds.put("publicmds", enable);
			ret.put("response", mds);
			
		}else {
			throw new CommandException("Unknown action : "+action);
		}
		
		return ret;
	}

	public void sendPendingResult(String zMiniDAPPID, String zPendinUID, boolean zAccept, boolean zStatus, JSONObject zResult) {
		
		//Is it a valid MiniDAPP
		if(!zMiniDAPPID.equals("")) {
			
			JSONObject res = new JSONObject();
			res.put("uid", zPendinUID);
			res.put("accept", zAccept);
			res.put("status", zStatus);
			res.put("result", zResult);
			
			Main.getInstance().PostNotifyEvent("MDS_PENDING", res, zMiniDAPPID);
		}
	}
	
	@Override
	public Command getFunction() {
		return new mds();
	}

}
