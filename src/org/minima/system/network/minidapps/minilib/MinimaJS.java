package org.minima.system.network.minidapps.minilib;

import org.minima.utils.MinimaLogger;
import org.mozilla.javascript.Function;

public class MinimaJS {
	
	BackBoneDAPP mBackBone;
	
	String mMiniDAPPID = "0x00";
	
	public MinimaJS(String zMiniDAPPID, BackBoneDAPP zBackBone) {
		mMiniDAPPID  = zMiniDAPPID;
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
	 * Main Command function
	 * 
	 * @param zCommand
	 */
	public void cmd(String zCommand) {
		cmd(zCommand,null);
	}
	
	public void cmd(String zCommand, Function zCallback) {
		MinimaLogger.log("MinimaJS command - "+zCommand+" "+zCallback);
		
		//Create a Command 
		Command cmd = new Command(zCommand, zCallback, mBackBone.getContext(), mBackBone.getScope());
		
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
}
