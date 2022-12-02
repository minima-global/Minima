package org.minima.system.mds.hub;

public class MDSHubLogon {

	public static String createHubPage(String zSessionID) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnLogonHeader();
		
		//Now the Login Form
		page += 
		"<center>"
		+ "<div class='mainlogon'>\n"
		+ "	<h2>MDS Login</h2>\n"
		+ "	<br>\n"
		+"	<form action='login.html' method='post'>\n"
		+ "	\n"
		+ "		<input type='hidden' name='sessionid' value='"+zSessionID+"'>\n"
		+ "		<input class='logonentry' type='password' name='password' required/>\n"
		+ "		\n"
		+ "		<input class='logonbutton' style='width:100;' type='submit' value='login' onClick=\"this.form.submit(); this.disabled=true; this.value='Checking..';\"/>\n"
		+ "		\n"
		+ "	</form>\n"
		+ "</div>"
		+ "<br>"
		+ "You may only open <b>1</b> MiniHUB at a time<br>"
		+ "</center>";

		
		page += MDSUtil.HUB_END;
		
		return page;
		
	}
}
