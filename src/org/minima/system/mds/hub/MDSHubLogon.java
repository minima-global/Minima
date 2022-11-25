package org.minima.system.mds.hub;

public class MDSHubLogon {

	public static String createHubPage(String zSessionID) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
				
		//Now the Login Form
		page += 
		"<center>"
		+ "<div class=\"mainlogon\">\n"
		+ "	<h2>MDS Login</h2>\n"
		+ "	<br>\n"
		+"	<form action=\"login.html\" method=\"post\">\n"
		+ "	\n"
		+ "		<input type='hidden' name='sessionid' value='"+zSessionID+"'>\n"
		+ "		<input type=\"password\" name=\"password\" required/>\n"
		+ "		\n"
		+ "		<input style=\"width:100;\" type=\"submit\" value=\"login\" onClick=\"this.form.submit(); this.disabled=true; this.value='Checking..';\"/>\n"
		+ "		\n"
		+ "	</form>\n"
		+ "</div>"
		+ "<br>"
		+ "You may only login to <b>1</b> MiniHUB at a time<br>"
		+ "<br>"
		+ "You will be logged out of others that are open"
		+ "</center>";

		
		page += MDSUtil.HUB_END;
		
		return page;
		
	}
}
