package org.minima.system.network.minidapps.minilib;

import org.minima.system.network.commands.NET;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeObject;

public class JSNet {

	/**
	 * JS BACKEND link
	 */
	private BackEndDAPP mBackBone;
	
	public JSNet(BackEndDAPP zBackBone) {
		mBackBone = zBackBone;
	}
	
	public void listen(int zPort) {
		//Create the command
		String command = "listen "+zPort;
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID());
		
		//Run it..
		func.run();		
	}
	
	public void stop(int zPort) {
		//Create the command
		String command = "stop "+zPort;
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID());
		
		//Run it..
		func.run();
	}
	
	public void broadcast(int zPort, Object zObject) {
		//Create a JSON
		JSONObject jobj = MiniLibUtility.toJsonObject((NativeObject)zObject);
		
		//Create the command
		String command = "braodcast "+zPort+" "+jobj.toString();
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID());
		
		//Run it..
		func.run();
	}
	
	public void connect(String zHostPort) {
		//Create the command
		String command = "connect "+zHostPort;
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID());
		
		//Run it..
		func.run();
	}
	
	public void disconnect(String zUID) {
		//Create the command
		String command = "disconnect "+zUID;
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID());
		
		//Run it..
		func.run();
	}
	
	public void send(String zUID, Object zObject) {
		//Create a JSON
		JSONObject jobj = MiniLibUtility.toJsonObject((NativeObject)zObject);
		
		//Create the command
		String command = "send "+zUID+" "+jobj.toString();
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID());
		
		//Run it..
		func.run();
	}
	
	public void info(Function zCallback) {
		//Create the command
		String command = "info";
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID(), zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		func.run();
	}
	
	public void get(String zURL, Function zCallback) {
		//Create the command
		String command = "get "+zURL;
		
		//Create the NET class
		NET func = new NET(command, mBackBone.getMiniDAPPID(), zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		func.run();
	}
}
