package org.minima.system.mds.hub;

public class MDSHubLoggedOn {

	public static String createHubPage(String zSessionID) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnHeader(zSessionID, false, false);
		
		//Add the Main page Link
		page += MDSUtil.returnSessionIDButton(zSessionID,"main.html","Main Menu");
		
		//Now the Login Form
		page += 
		"<center>"
		+ "Log on Successful..!"
		+ "</center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
	}
}
