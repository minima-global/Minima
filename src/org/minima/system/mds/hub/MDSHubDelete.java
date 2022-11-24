package org.minima.system.mds.hub;

import org.minima.system.mds.MDSManager;

public class MDSHubDelete {
	
	public static String createHubPage(MDSManager zMDS, String zPassword) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		//Now the return form..
		page += MDSUtil.returnPasswordButton(zPassword,"login.html","Main Menu");
				
		page += "<br><div class='app-title'>MiniDAPP Deleted..</div><br><br>";
		
		page += MDSUtil.HUB_END;
		
		return page;
	}
}
