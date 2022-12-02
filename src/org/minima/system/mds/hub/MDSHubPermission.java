package org.minima.system.mds.hub;

import org.minima.system.mds.MDSManager;

public class MDSHubPermission {
	
	public static String createHubPage(MDSManager zMDS, String zSessionID, String zPermission) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		//Now the return form..
		page += MDSUtil.returnHeader(zSessionID, true, true);
				
		page += "<br><center><div class='app-title'>MiniDAPP given "+zPermission+" permissions..</div></center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
	}
}
