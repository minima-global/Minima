package org.minima.system.network.minidapps.minibackend;

import org.minima.system.Main;
import org.minima.system.network.commands.CMD;
import org.minima.system.network.commands.FILE;
import org.minima.system.network.commands.NET;
import org.minima.system.network.commands.SQL;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.utils.messages.Message;
import org.mozilla.javascript.Function;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class MinimaJSBridge {

	/**
	 * JS BACKEND link
	 */
	private BackEndDAPP mBackBone;
	
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public MinimaJSBridge(BackEndDAPP zBackBone) {
		mBackBone = zBackBone;
	}

	//When posting a reply to a intra minidapp message
	public void wspostreply(String zReplyID, String zMessage) {
		//Post a message..
		Message replymsg = new Message(DAPPManager.DAPP_DIRECTREPLY);
		replymsg.addString("replyid", zReplyID);
		replymsg.addString("message", zMessage);
		
		//Send it to the DAPP MANAGER
		Main.getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(replymsg);
	}
	
	public void post(String zType, String zData, Function zCallback) {
		
		//MinimaLogger.log("MinimaJSBridge : "+zType+" "+zData);
		
		String finalresult = "{}";
		
		//Is this a SQL function
		if(zType.equals("sql")) {
			//Create a SQL object
			SQL sql = new SQL(zData, mBackBone.getMiniDAPPID());
			
			//Run it..
			sql.run();
			
			//Get the Response..
        	finalresult = sql.getFinalResult();
			
		}else if(zType.equals("cmd")) {
			CMD cmd = new CMD(zData);
        	
        	//Run it..
            cmd.run();
 
            //Get the Response..
        	finalresult = cmd.getFinalResult();
		
		}else if(zType.equals("file")) {
			//File access..
			FILE file = new FILE(zData,  mBackBone.getMiniDAPPID());
			
			//Run it..
			file.run();
			
			//Get the Response..
        	finalresult = file.getFinalResult();
		
		}else if(zType.equals("net")) {
			//Network Comms
			NET netcomm = new NET(zData,  mBackBone.getMiniDAPPID());
			
			//Run it..
			netcomm.run();
			
			//Get the Response..
        	finalresult = netcomm.getFinalResult();
		}
	    
		//Use the Callback ?
		if(zCallback != null) {
			//Create a native JSON
			Object json = MiniJSONUtility.makeJSONObject(finalresult, mBackBone.getContext(), mBackBone.getScope());
			
			//Make a function variable list
			Object functionArgs[] = { json };
		    
			//Call the function..
			zCallback.call(mBackBone.getContext(), mBackBone.getScope(), mBackBone.getScope(), functionArgs);	
		}
	    
	}
}
