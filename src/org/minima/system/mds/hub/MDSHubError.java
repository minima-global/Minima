package org.minima.system.mds.hub;

public class MDSHubError {

public static String createHubPage() {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnHeader("", false, false);
		
		//Now the Login Form
		page += 
		"<center>"
		+ "Login / SessionID Error - pls log back in"
		+ "</center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
	}
}
