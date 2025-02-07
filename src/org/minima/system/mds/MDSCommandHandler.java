package org.minima.system.mds;

import java.io.UnsupportedEncodingException;

import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.mds.handler.APIAutoResponse;
import org.minima.system.mds.handler.APICommand;
import org.minima.system.mds.handler.CMDcommand;
import org.minima.system.mds.handler.COMMSCommand;
import org.minima.system.mds.handler.FILEcommand;
import org.minima.system.mds.handler.KEYPAIRcommand;
import org.minima.system.mds.handler.NETcommand;
import org.minima.system.mds.handler.NOTIFYcommand;
import org.minima.system.mds.handler.POLLcommand;
import org.minima.system.mds.handler.SQLcommand;
import org.minima.system.mds.polling.PollStack;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class MDSCommandHandler {

	/**
	 * The MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * The Polling Stack
	 */
	PollStack mPollStack;
	
	public MDSCommandHandler(MDSManager zMDS, PollStack zPStack) {
		mMDS 		= zMDS;
		mPollStack 	= zPStack;
	}
	
	public String runCommand(String minidappid, String command, String data) throws UnsupportedEncodingException, MDSCommandException {
		
		String result = null;
		if(command.equals("sql")) {
		
			SQLcommand sql = new SQLcommand(mMDS);
			result = sql.runCommand(minidappid, data);
			
		}else if(command.equals("cmd")) {
			
			//Create a Command and run it..
			CMDcommand cmd = new CMDcommand(minidappid, data);
			result = cmd.runCommand();

		}else if(command.equals("notify")) {
			
			//Get the MiniDAPP
			MiniDAPP md = mMDS.getMiniDAPP(minidappid);
			String name = md.getName(); 
			
			//Create a Command and run it..
			NOTIFYcommand notify = new NOTIFYcommand(minidappid, name, data, true);
			result = notify.runCommand();

		}else if(command.equals("notifycancel")) {
			
			//Create a Command and run it..
			NOTIFYcommand notify = new NOTIFYcommand(minidappid, "", "", false);
			result = notify.runCommand();
			
		}else if(command.equals("net")) {
			
			//Create a Command and run it..
			NETcommand net 	= new NETcommand(minidappid, data);
			result 			= net.runCommand();
		
		}else if(command.equals("netauth")) {
			
			//Get the URL and the headers..
			int dataindex 		= data.indexOf("&");
			String url 			= data.substring(0, dataindex);
			String authtoken  	= data.substring(dataindex+1);
			
			//Create a Command and run it..
			NETcommand net 	= new NETcommand(minidappid, url, "", authtoken);
			result 			= net.runCommand();
			
		}else if(command.equals("netpost")) {
			
			//Get the URL and the post data..
			int dataindex 	= data.indexOf("&");
			String url 		= data.substring(0, dataindex);
			String postdata = data.substring(dataindex+1);
			
			//Create a Command and run it..
			NETcommand net 	= new NETcommand(minidappid,url, postdata);
			result 			= net.runCommand();
		
		}else if(command.equals("keypair")) {
			
			//Get the URL and the post data..
			int dataindex 	= data.indexOf("&");
			String action 	= data.substring(0, dataindex);
			String keydata 	= data.substring(dataindex+1);
			
			//Is it set or get
			KEYPAIRcommand kp = null;
			if(action.equals("get")) {
			
				kp = new KEYPAIRcommand(mMDS, minidappid, KEYPAIRcommand.KEYPAIR_GET,keydata,"");
			}else {
				
				//It's a set
				dataindex 		= keydata.indexOf("&");
				String key 		= keydata.substring(0, dataindex);
				String value 	= keydata.substring(dataindex+1);
				
				kp = new KEYPAIRcommand(mMDS, minidappid, KEYPAIRcommand.KEYPAIR_SET,key,value);
			}
			
			//Run it
			result = kp.runCommand();
		
		}else if(command.equals("file")) {
			
			//Get the URL and the post data..
			int dataindex 	= data.indexOf("&");
			String action 	= data.substring(0, dataindex);
			String filedata = data.substring(dataindex+1);
			
			//What was the data
			FILEcommand fc = null;
			if(action.equals("list")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_LIST, filedata, "");
			
			}else if(action.equals("save")) {
				dataindex 			= filedata.indexOf("&");
				String file 		= filedata.substring(0, dataindex);
				String actualedata 	= filedata.substring(dataindex+1);
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_SAVE, file, actualedata);
			
			}else if(action.equals("savebinary")) {
				dataindex 			= filedata.indexOf("&");
				String file 		= filedata.substring(0, dataindex);
				String actualedata 	= filedata.substring(dataindex+1);
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_SAVEBINARY, file, actualedata);
			
			}else if(action.equals("load")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_LOAD, filedata, "");
			
			}else if(action.equals("loadbinary")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_LOADBINARY, filedata, "");
			
			}else if(action.equals("delete")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_DELETE, filedata, "");
			
			}else if(action.equals("getpath")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_GETPATH, filedata, "");
			
			}else if(action.equals("makedir")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_MAKEDIR, filedata, "");
			
			}else if(action.equals("copy")) {
				dataindex 			= filedata.indexOf("&");
				String file 		= filedata.substring(0, dataindex);
				String copyfile 	= filedata.substring(dataindex+1);
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_COPY, file, copyfile);
			
			}else if(action.equals("move")) {
				dataindex 			= filedata.indexOf("&");
				String file 		= filedata.substring(0, dataindex);
				String movefile 	= filedata.substring(dataindex+1);
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_MOVE, file, movefile);
			
			}else if(action.equals("download")) {
				
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_DOWNLOAD, filedata, "");
				
			}else if(action.equals("copytoweb")) {
				
				dataindex 			= filedata.indexOf("&");
				String file 		= filedata.substring(0, dataindex);
				String movefile 	= filedata.substring(dataindex+1);
				
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_COPYTOWEB, file, movefile);
				
			}else if(action.equals("deletefromweb")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_DELETEFROMWEB, filedata, "");
			
			}else if(action.equals("listweb")) {
				fc = new FILEcommand(mMDS, minidappid, 
						FILEcommand.FILECOMMAND_LISTWEB, filedata, "");
				
			}else {
				throw new MDSCommandException("Invalid function : "+action);
			}
			
			//Create a Command and run it..
			result = fc.runCommand();
		
		}else if(command.equals("comms")) {
			
			//Is it public or private
			int dataindex 	= data.indexOf("&");
			String pubpriv 	= data.substring(0, dataindex);
			String msg 		= data.substring(dataindex+1);
			
			//Get the Name of the MiniDAPP..
			MiniDAPP md = mMDS.getMiniDAPP(minidappid);
			
			//Create a Command and run it..
			if(pubpriv.equals("public")) {
				COMMSCommand comms = new COMMSCommand(mMDS, "*", md.getName(),  msg);
				result = comms.runCommand();
			}else {
				COMMSCommand comms = new COMMSCommand(mMDS, minidappid, md.getName(), msg);
				result = comms.runCommand();
			}
		
		}else if(command.equals("api")) {
			
			//Get the Name of the MiniDAPP..
			MiniDAPP thismd = mMDS.getMiniDAPP(minidappid);
			
			//Is it public or private
			int dataindex 	= data.indexOf("&");
			int dataindex2 	= data.indexOf("&",dataindex+1);
			int dataindex3 	= data.indexOf("&",dataindex2+1);
			
			String mininame	= data.substring(0, dataindex);
			String type		= data.substring(dataindex+1, dataindex2);
			String randid	= data.substring(dataindex2+1, dataindex3);
			String msg 		= data.substring(dataindex3+1);
			
			//MinimaLogger.log("mini:"+mininame+" type:"+type+" randid:"+randid+" msg:"+msg);
			
			//Get the Name of the MiniDAPP..
			MiniDAPP md = mMDS.getMiniDAPPFromName(mininame);
			
			//Do we have that MiniDAPP..
			if(md == null) {
				
				//No MiniDAPP.. send auto response..
				APIAutoResponse auto = new APIAutoResponse(mMDS, mininame, 
						thismd.getName(), thismd.getUID(),  randid);
				auto.setImmediate();
				auto.runauto();
				
			}else if(type.equals("request")){
				APICommand comms = new APICommand(mMDS, thismd.getName(), 
						md.getName(), md.getUID(),  msg, randid, true);
				result = comms.runCommand();
				
				//And start a thread that calls the API in 5 secs if nothing else..
				APIAutoResponse auto = new APIAutoResponse(mMDS, md.getName(), 
						thismd.getName(), thismd.getUID(),  randid);
				auto.runauto();
				
			}else {
				APICommand comms = new APICommand(mMDS, thismd.getName(), 
						md.getName(), md.getUID(), msg, randid, false);
				result = comms.runCommand();
			}
			
		}else if(command.equals("dapplink")) {
			
			//Not allowed on Public MiniDAPP
			boolean publicmini = (minidappid == mMDS.getPublicMiniDAPPID()) || 
								 (minidappid == mMDS.getUntrustedMiniDAPPID());
			
			//Get the MiniDapp in question..
			MiniDAPP reqmini = mMDS.getMiniDAPPFromName(data.trim());
			if(reqmini == null || publicmini) {
				//No MiniDAPP found..
				JSONObject error = new JSONObject();
				error.put("status", false);
				error.put("error", "MiniDAPP with that name not found");
				result = error.toString();
			}else {
				
				//Are permission levels intact
				boolean allow = false;
				
				//Check the Permissions..
				if(reqmini.getPermission().equalsIgnoreCase("read")) {
					allow = true;
				}else {
					
					//Check OUR permission..
					MiniDAPP md = mMDS.getMiniDAPP(minidappid);
					if(md.getPermission().equalsIgnoreCase("write")) {
						allow = true;
					}
				}
				
				//Ok to send link.. ?
				if(allow) {
					
					//Get the sessionid..
					String sessionid = mMDS.convertMiniDAPPID(reqmini.getUID());
					
					//And return the data
					JSONObject linkresult = new JSONObject();
					linkresult.put("status", true);
					
					JSONObject resp = new JSONObject();
					resp.put("uid", reqmini.getUID());
					resp.put("sessionid", sessionid);
					
					linkresult.put("response", resp);
					result = linkresult.toString();
					
				}else {
					//No MiniDAPP found..
					JSONObject error = new JSONObject();
					error.put("status", false);
					error.put("error", "MiniDAPP permission escalation");
					result = error.toString();
				}
			}
			
		}else if(command.equals("poll")) {
			
			POLLcommand poll = new POLLcommand(mPollStack);
			result = poll.runCommand(minidappid, data);
			
		}else{
			
			//Is it a CMD / SQL / FILE / FUNC ..
			MinimaLogger.log("ERROR MDS COMPLETE HANDLER REQ : "+command);
			
			//Invalid command
			JSONObject error = new JSONObject();
			error.put("status", false);
			
			result = error.toString(); 
		}
		
		return result;
	}
	
}
