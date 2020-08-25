package org.minima.system.network.commands;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.StringTokenizer;

import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.system.network.minidapps.comms.CommsClient;
import org.minima.system.network.minidapps.comms.CommsManager;
import org.minima.system.network.minidapps.comms.CommsServer;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class NET implements Runnable {

	//The Command to run
	String mCommand;
	String mMiniDAPPID;
	
	//The Final Result..
	String mFinalResult = "";
	
	public NET(String zCommand, String zMiniDAPPID) {
		mCommand    = zCommand.trim();
		mMiniDAPPID = zMiniDAPPID;
	}

	public String getFinalResult() {
		return mFinalResult;
	}

	@Override
	public void run() {
		//Get a handle on something
		InputHandler input = Main.getMainHandler().getInputHandler();
		DAPPManager dappm  = input.getMainHandler().getNetworkHandler().getDAPPManager();
		CommsManager comms = dappm.getCommsManager();
		
		JSONObject resp = new JSONObject();
		resp.put("function", mCommand);
		
		StringTokenizer strtok = new StringTokenizer(mCommand," ");
		String command = strtok.nextToken();
        
		//Which Command is it..
		if(mCommand.startsWith("listen ")) {
			int port = Integer.parseInt(strtok.nextToken());
			
			//Start a Comms Server on this port..
			Message starter = new Message(CommsManager.COMMS_START);
			starter.addInteger("port", port);
			starter.addString("minidappid", mMiniDAPPID);
			
			//Post it..
			comms.PostMessage(starter);
			
		}else if(mCommand.startsWith("stop ")) {
			int port = Integer.parseInt(strtok.nextToken());
			
			//Stop this Comms Server
			Message stopper = new Message(CommsManager.COMMS_STOP);
			stopper.addInteger("port", port);
			stopper.addString("minidappid", mMiniDAPPID);
			
			//Post it..
			comms.PostMessage(stopper);
			
		}else if(mCommand.startsWith("broadcast ")) {
			int port = Integer.parseInt(strtok.nextToken());
			
			int index    = mCommand.indexOf(" ",11);
			String json  = mCommand.substring(index).trim();
			
			//Broadcast a message to everyone on this server
			Message broadcast = new Message(CommsManager.COMMS_BROADCAST);
			broadcast.addInteger("port", port);
			broadcast.addString("minidappid", mMiniDAPPID);
			broadcast.addString("message", json);
			
			//Post it..
			comms.PostMessage(broadcast);
		
		}else if(mCommand.startsWith("connect ")) {
			String hostport = strtok.nextToken();
			
			//Stop this Comms Server
			Message stopper = new Message(CommsManager.COMMS_CONNECT);
			stopper.addString("hostport", hostport);
			stopper.addString("minidappid", mMiniDAPPID);
			
			//Post it..
			comms.PostMessage(stopper);
		
		}else if(mCommand.startsWith("disconnect ")) {
			String uid = strtok.nextToken();
			
			//Stop this Comms Server
			Message stopper = new Message(CommsManager.COMMS_DISCONNECT);
			stopper.addString("uid", uid);
			stopper.addString("minidappid", mMiniDAPPID);
			
			//Post it..
			comms.PostMessage(stopper);
		
		}else if(mCommand.startsWith("send ")) {
			String uid = strtok.nextToken();
			
			int index    = mCommand.indexOf(" ",6);
			String json  = mCommand.substring(index).trim();
			
			//Broadcast a message to everyone on this server
			Message sender = new Message(CommsManager.COMMS_SEND);
			sender.addString("uid", uid);
			sender.addString("minidappid", mMiniDAPPID);
			sender.addString("message", json);
			
			//Post it..
			comms.PostMessage(sender);	
		
		}else if(mCommand.startsWith("info")) {
			
			JSONArray sarr = new JSONArray();
			ArrayList<CommsServer> servers = comms.getServers();
			for(CommsServer server : servers) {
				sarr.add(new Integer(server.getPort()));
			}
			resp.put("servers", sarr);
			
			JSONArray carr = new JSONArray();
			ArrayList<CommsClient> clients = comms.getClients();
			for(CommsClient client : clients) {
				carr.add(client.toJSON());
			}
			resp.put("clients", carr);	
		
		}else if(mCommand.startsWith("get ")) {
			String url = strtok.nextToken();
			
			try {
				String result = RPCClient.sendGET(url);
				resp.put("result", URLEncoder.encode(result,"UTF-8"));
				
			} catch (IOException e) {
				MinimaLogger.log("Error NET GET : "+url+" "+e);
				resp.put("error", e.toString());
			}
		
		}else {
			resp.put("error", "UNKNOWN COMMAND");
		}
		
		//Stroe for pickup
		mFinalResult = resp.toString();
		
		//Now send the result back vis the callback..
//		if(mCallback != null) {
//			//Create a native JSON
//			Object json = MiniJSONUtility.makeJSONObject(mFinalResult, mContext, mScope);
//			
//			//Make a function variable list
//			Object functionArgs[] = { json };
//		    
//			//Call the function..
//			mCallback.call(mContext, mScope, mScope, functionArgs);
//		}
	}

	
}
