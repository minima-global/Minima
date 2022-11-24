package org.minima.system.mds.hub;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.mds.MDSManager;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.utils.json.JSONObject;

public class MDSHubPending {

	public static String createHubPage(MDSManager zMDS, String zPassword) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		page += MDSUtil.returnPasswordButton(zPassword,"login.html","Main Menu");
		
		//Get all the pending commands..
		ArrayList<PendingCommand> allpending = Main.getInstance().getMDSManager().getAllPending(); 
		
		if(allpending.size() == 0) {
			page += "<li><div class='list-item-container'>No pending transactions..<br><br></div></li>";
		}else {
			
			for(PendingCommand pcommand : allpending) {
				
				//Get the MiniDAPP
				JSONObject md 	= pcommand.getMiniDAPP();
				JSONObject conf = (JSONObject) md.get("conf");
				
				//Get the details
				String name 	= conf.getString("name");
				String icon 	= conf.getString("icon");
				String command 	= pcommand.getCommand();
				String puid 	= pcommand.getUID();
				
				String base = "./"+md.getString("uid")+"/";
				
				page +=   "<li>\n"
						+ ""
						+ "				<div class=\"list-item-container\">"
						+ "                    <img width='50' height='50' src='"+base+icon+"'>\n"
						+ "\n"
						+ "                    <div class=list-item-right>\n"
						+ "                        <div class=\"app-title\">"+MDSHub.stripHTML(name)+"</div>\n"
						+ ""
						+ "                        <div><br><br><br>\r\n"
						+ "                        	Command :<br><br>\r\n"
						+ "                        	<table border=0 width=100%>\r\n"
						+ "                        		<tr>\r\n"
						+ "                        			<td style=\"font-size:12;\">"+MDSHub.stripHTML(command)+"</td>\r\n"
						+ "                        		</tr>\r\n"
						+ "                        	</table>\r\n"
						+ "                        \r\n"
						+ "                        </div>"
						+ ""
						+ "						   <div style=\"height:40\">\r\n"
						+ "                        	<br>\r\n"
						+ "                        	<table>\r\n"
						+ "                        		<tr>\r\n"
						+ "                        			\r\n"
						+ "                        			<td>\r\n"
						+ "                        				<form action=\"pendingaction.html\" method=POST onsubmit=\"return confirm('Are you sure you want to ACCEPT this action ?');\">\r\n"
						+ "										  <input type='hidden' name='password' value='"+zPassword+"'>\r\n"
						+ "										  <input type='hidden' name='accept' value='accept'>\r\n"
						+ "										  <input type='hidden' name='uid' value='"+puid+"'>\r\n"
						+ "										  <input class='acceptbutton' type=\"submit\" value='Accept'>\r\n"
						+ "										</form>		\r\n"
						+ "                        			</td>\r\n"
						+ "									<td>&nbsp;&nbsp;</td>"
						+ "                        			<td>\r\n"
						+ "                        				<form action=\"pendingaction.html\" method=POST onsubmit=\"return confirm('Are you sure you want to DENY this action ?');\">\r\n"
						+ "										  <input type='hidden' name='password' value='"+zPassword+"'>\r\n"
						+ "										  <input type='hidden' name='accept' value='deny'>\r\n"
						+ "										  <input type='hidden' name='uid' value='"+puid+"'>\r\n"
						+ "										  <input class='deletebutton' type=\"submit\" value='Deny'>\r\n"
						+ "										</form>		\r\n"
						+ "                        			</td>\r\n"
						+ "                        			\r\n"
						+ "                        		</tr>\r\n"
						+ "                        	</table>\r\n"
						+ "                        </div>"
						+ ""
						+ "                    </div>\n"
						+ "                </div>\n"
						+ "</li>";
			}
		}
	
		//End the HTML
		page += MDSUtil.HUB_END;
		
		return page;
		
	}
}
