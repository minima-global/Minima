package org.minima.system.mds.handler;

import org.minima.system.mds.MDSManager;

public class SQLcommand {

	MDSManager mMDS;
	
	public SQLcommand(MDSManager zManager) {
		mMDS = zManager;
	}
	
	public String runCommand(String zMiniDAPPID, String zSQL) {
		return mMDS.runSQL(zMiniDAPPID, zSQL).toString();
	}
}
