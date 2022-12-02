package org.minima.system.mds.hub;

import org.minima.system.mds.MDSManager;

public class MDSHubDelete {
	
	public static String createHubPage(MDSManager zMDS, String zSessionID) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnHeader(zSessionID, true, true);
				
		page += "<br><center><div class='app-title'>MiniDAPP Deleted..</div></center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
	}
}
