package org.minima.system.mds.hub;

import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.mds.MDSManager;

public class MDSHubInstall {

	public static String createHubPage(MDSManager zMDS, MiniDAPP zDAPP, String zSessionID) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnHeader(zSessionID, true, true);
		
		String base = "./"+zDAPP.getUID()+"/";
		
		page +=   "<li>\n"
				+ "                <div class=\"list-item-container\">\n"
				+ "                    <img width='50' height='50' src='"+base+zDAPP.getIcon()+"'>\n"
				+ "\n"
				+ "                    <div class=list-item-right>\n"
				+ "                        <div class=\"app-title\">"+MDSHub.stripHTML(zDAPP.getName())+"</div>\n"
				+ "                        <div>"+MDSHub.stripHTML(zDAPP.getDescription())+"</div>\n"
				+ "                        <div>"+MDSHub.stripHTML(zDAPP.getVersion())+"</div>\n"
				+ "                    </div>\n"
				+ "                </div>\n"
				+ "</li>";
		
		page += "<br><center><div class='app-title'>MiniDAPP Installed..</div></center>";
		
		page += MDSUtil.HUB_END;
		
		return page;
		
	}
}
