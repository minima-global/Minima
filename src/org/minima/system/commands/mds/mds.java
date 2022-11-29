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
				+ "action: (optional)\n"
				+ "    list : List your installed MiniDapps. Default parameter.\n"
				+ "    install : Install a new MiniDapp and optionally set its permission. Must specify 'file'.\n"
				+ "    update : Update and replace an existing MiniDapp. Must specify MiniDapp 'uid' and 'file' of new MiniDapp.\n"
				+ "    uninstall : Uninstall a MiniDapp. Must specify MiniDapp 'uid'.\n"
				+ "    pending : List all pending commands waiting to be accepted or denied.\n"
				+ "    accept : Accept a pending command. Must specify 'uid' of the pending command.\n"
				+ "    deny : Deny a pending command. Must specify 'uid' of the pending command.\n"
				+ "    permission : Set permission for a MiniDapp to READ or WRITE. Must specify existing MiniDapp 'uid' and 'trust'.\n"
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
				+ "mds action:pending\n"
				+ "\n"
				+ "mds action:accept uid:0xCDF6..\n"
				+ "\n"
				+ "mds action:deny uid:0xCDF6..\n"
				+ "\n"
				+ "mds action:permission uid:0xABA3.. trust:write\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","file","uid","trust"}));
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

			JSONObject mds = new JSONObject();
			mds.put("enabled", GeneralParams.MDS_ENABLED);
			mds.put("connect", "https://"+GeneralParams.MINIMA_HOST+":"+GeneralParams.MDSFILE_PORT);
			mds.put("password", Main.getInstance().getMDSManager().getMiniHUBPasword());
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
			
			//Get all the pending commands..
			ArrayList<PendingCommand> allpending = Main.getInstance().getMDSManager().getAllPending(); 
			
			//Make into JSONArray
			boolean found = false;
			for(PendingCommand pending : allpending) {
				if(pending.getUID().equals(uid)) {
					
					//RUN it.. as normal 0x00 - so is accepted
					CMDcommand cmd 	= new CMDcommand("0x00", pending.getCommand());
					String result 	= cmd.runCommand();
					
					if(result.startsWith("{")) {
						ret.put("response", (JSONObject) new JSONParser().parse(result));
					
					}else if(result.startsWith("[")) {
						ret.put("response", (JSONArray) new JSONParser().parse(result));
					
					}else {
						ret.put("response", result);
					}
					
					found = true;
					break;
				}
			}
			
			//Did we find it..
			if(found) {
				//Remove it from the list
				Main.getInstance().getMDSManager().removePending(uid);
			}else {
				throw new CommandException("Pending UID not found : "+uid);
			}
			
		}else if(action.equals("deny")) {
			
			//Get the uid
			String uid = getParam("uid");
			
			//Remove it from the list
			boolean found = Main.getInstance().getMDSManager().removePending(uid);
			if(!found) {
				throw new CommandException("Pending UID not found : "+uid);
			}
			
			ret.put("response", "Pending action removed : "+uid);
			
		}else if(action.equals("install")) {
		
			String file = getParam("file");
			
			File minidapp = MiniFile.createBaseFile(file);
			if(!minidapp.exists()) {
				throw new CommandException("MiniDAPP not found.. : "+file);
			}
			
			//Now start
			FileInputStream fis = new FileInputStream(minidapp);
			
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
			
			JSONObject mds = new JSONObject();
			mds.put("installed", md.toJSON());
			ret.put("response", mds);
			
			//There has been a change
			Message installed = new Message(MDSManager.MDS_MINIDAPPS_INSTALLED);
			installed.addObject("minidapp", md);
			Main.getInstance().getMDSManager().PostMessage(installed);
			
		}else if(action.equals("uninstall")) {

			String uid = getParam("uid");
			if(!uid.startsWith("0x")) {
				throw new CommandException("Invalid UID for MiniDAPP");
			}
			
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
			
			//Get the MIninDAPP..
			MiniDAPP md = db.getMiniDAPP(uid);
			if(md == null) {
				throw new CommandException("MiniDAPP not found : "+uid);
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
					db.deleteMiniDAPP(uid);
					
					//Load the conf file..
					MiniString data = new MiniString(MiniFile.readCompleteFile(new File(dapp,"dapp.conf"))); 	
					
					//Now create the JSON..
					JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
					
					//Create the MiniDAPP
					MiniDAPP md = new MiniDAPP(uid, jsonconf);
					
					//Now add to the DB
					db.insertMiniDAPP(md);
					
					//Add to the final array
					arr.add(md.toJSON());
				}
			}
			
			JSONObject mds = new JSONObject();
			mds.put("minidapps", arr);
			ret.put("response", mds);
			
			//There has been a change
			Main.getInstance().getMDSManager().PostMessage(MDSManager.MDS_MINIDAPPS_RESETALL);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new mds();
	}

}
