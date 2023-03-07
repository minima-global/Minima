package org.minima.system.mds.hub;

import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.mds.MDSManager;

public class MDSHubInstallError {

	public static String createHubPage(MDSManager zMDS, String zSessionID, String zError) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnHeader(zSessionID, true, true);
		
		page += "<br><center><div class='app-title'>MiniDAPP FAILED to install..</div>"
				+ "<br>"
				+ zError
				+ "</center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
		
	}
}
