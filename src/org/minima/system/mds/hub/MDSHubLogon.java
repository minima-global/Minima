package org.minima.system.mds.hub;

public class MDSHubLogon {

	public static final String HUB_START ="<html>\r\n"
			+ "\r\n"
			+ "<head>\r\n"
			+ "	<title>MDS Login</title>\r\n"
			+ "	\r\n"
			+ "	<style type=\"text/css\">\r\n"
			+ "	\r\n"
			+ "	body {\r\n"
			+ "            font-family: Arial, sans-serif;\r\n"
			+ "            margin: 0;\r\n"
			+ "        }\r\n"
			+ "        \r\n"
			+ "	.mainlogon {\r\n"
			+ "            padding: 20px;\r\n"
			+ "            background: #F4F4F5;\r\n"
			+ "            border-radius: 16px;\r\n"
			+ "            color: #16181C;\r\n"
			+ "            text-decoration: none;\r\n"
			+ "            width : 300px;\r\n"
			+ "        }\r\n"
			+ "	\r\n"
			+ "	</style>\r\n"
			+ "	\r\n"
			+ "</head>\r\n"
			+ "\r\n"
			+ "<body>\r\n"
			+ "\r\n"
			+ "<br><br><br>\r\n"
			+ "\r\n"
			+ "<center>\r\n"
			+ "\r\n"
			+ "<div class=\"mainlogon\">\r\n"
			+ "\r\n"
			+ "	<h2>MDS Login</h2>\r\n"
			+ "	<br>\r\n"
			+ "	\r\n"
			+ "	<form action=\"login.html\" method=\"post\">\r\n"
			+ "	\r\n"
			+ "		<input type=\"password\" name=\"password\"/>\r\n"
			+ "		\r\n"
			+ "		<input style=\"width:100;\" type=\"submit\" value=\"login\" onClick=\"this.form.submit(); this.disabled=true; this.value='Checking…';\"/>\r\n"
			+ "		\r\n"
			+ "	</form>\r\n"
			+ "\r\n"
			+ "</div>\r\n"
			+ "\r\n"
			+ "</center>\r\n"
			+ "\r\n"
			+ "</body>\r\n"
			+ "\r\n"
			+ "</html>";
	
	public static String createHubPage() {
		
		//Start the HTML
		String page = HUB_START;
				
		return page;
		
	}
}
