package org.minima.system.mds.hub;

public class MDSHubError {

public static String createHubPage() {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		//Add the Main page Link
		page += MDSUtil.returnSessionIDButton("","index.html","Back to Logon");
		
		//Now the Login Form
		page += 
		"<center>"
		+ "Login / SessionID Error - pls log back in"
		+ "</center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
	}
}
