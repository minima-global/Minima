package org.minima.system.network.minidapps.minilib;

import org.minima.utils.MinimaLogger;
import org.mozilla.javascript.Function;

public class MinimaJS {
	
	/**
	 * File functions..
	 */
	public JSFile file;
	
	/**
	 * JS BACKEND link
	 */
	private BackEndDAPP mBackBone;
	
	public MinimaJS(BackEndDAPP zBackBone) {
		mBackBone    = zBackBone;
	}
	
	/**
	 * Log data to Standard out
	 * @param zLog
	 */
	public void log(String zLog) {
		MinimaLogger.log("MinimaJS log - "+zLog);
	}
	
	/**
	 * Main Minima Command
	 * 
	 * @param zCommand
	 */
	public void cmd(String zCommand) {
		cmd(zCommand,null);
	}
	
	public void cmd(String zCommand, Function zCallback) {
		MinimaLogger.log("MinimaJS command - "+zCommand+" "+zCallback);
		
		//Create a Command 
		Command cmd = null;
		if(zCallback != null) {
			cmd = new Command(zCommand, zCallback, mBackBone.getContext(), mBackBone.getScope());	
		}else {
			cmd = new Command(zCommand);
		}

		//Run it..
		Thread cmdthread = new Thread(cmd);
		cmdthread.start();
	}
	
	
	/**
	 * Main SQL function
	 * 
	 * @param zCommand
	 */
	public void sql(String zCommand) {
		sql(zCommand, null);
	}
	
	public void sql(String zCommand, Function zCallback) {
		MinimaLogger.log("MinimaJS sql -"+zCommand);
	}
	
	/**
	 * Backend / Frontend comms - Fires a POST MinimaEvent
	 */
	public void post(Object zObject) {}
	
	/**
	 * Intra-MiniDAPP comms.
	 * 
	 * @param zMinDAPPID
	 * @param zObject
	 */
	public void send(String zMinDAPPID, Object zMessage) {}
	public void reply(Object zOriginalMessage, Object zReply) {}	
}
