package org.minima.system.mds.hub;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;

public class MDSHubError {

	public static final String HUB_START ="<html>\n"
			+ "\n"
			+ "<head>\n"
			+ "	<title>MDS Logon</title>\n"
			+ "</head>\n"
			+ "\n"
			+ "<body>\n"
			+ "\n"
			+ "<br><br>\n"
			+ "\n"
			+ "<center>\n"
			+ "\n"
			+ "<h2>MDS Login</h2>\n"
			+ "\n"
			+ "<h3>Error Login - please try again</h3>\n"
			+ "\n"
			+ "<a href='./'>Back to Login</a>\n"
			+ "\n"
			+ "</center>\n"
			+ "\n"
			+ "</body>\n"
			+ "\n"
			+ "</html>";
	
	public static String createHubPage() {
		
		//Start the HTML
		String page = HUB_START;
				
		return page;
		
	}
}
