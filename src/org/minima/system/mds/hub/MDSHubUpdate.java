package org.minima.system.mds.hub;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.Main;
import org.minima.system.mds.MDSManager;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.utils.json.JSONObject;

public class MDSHubUpdate {

	public static String createHubPage(MDSManager zMDS, String zSessionID, String zMiniDAPPID) {

		MiniDAPP dapp 	= MinimaDB.getDB().getMDSDB().getMiniDAPP(zMiniDAPPID); 
		String base 	= "./"+dapp.getUID()+"/";
		
		
		//Start the HTML
		String page = MDSUtil.HUB_START;

		page += MDSUtil.returnHeader(zSessionID, true, true);
		
		//And the Update MiniDAPP thing..
		page +=   "<li>\n"
				+ "                <div class=\"list-item-container\">\n"
				+ "        			<img width='50' height='50' src='"+base+dapp.getIcon()+"'>\n"
				+ "\n"
				+ "                    <div class=list-item-right>\n"
				+ "                        <div class=\"app-title\">Update "+MDSHub.stripHTML(dapp.getName())+"</div>\n"
				+ "                        <div><br>"
				+ "<form action=\"updateminidapp.html\" method=POST enctype='multipart/form-data'>\n"
				+ "  <input type='hidden' name='sessionid' value='"+zSessionID+"'>\n"
				+ "  <input type='hidden' name='minidappid' value='"+zMiniDAPPID+"'>\n"
				+ "  <input class='solobutton' type=\"file\" id=\"myFile\" name=\"filename\" required>\n"
				+ "  <input class='solobutton' type=\"submit\" value='&nbsp;Update&nbsp;'>\n"
				+ "</form>"
				+ "							</div>\n"
				+ "                    </div>\n"
				+ "                </div>\n"
				+ "            </li>";
		
//		//Get all the pending commands..
//		ArrayList<PendingCommand> allpending = Main.getInstance().getMDSManager().getAllPending(); 
//		
//		if(allpending.size() == 0) {
//			page += "<li><div class='list-item-container'>No pending transactions..<br><br></div></li>";
//		}else {
//			
//			for(PendingCommand pcommand : allpending) {
//				
//				//Get the MiniDAPP
//				JSONObject md 	= pcommand.getMiniDAPP();
//				JSONObject conf = (JSONObject) md.get("conf");
//				
//				//Get the details
//				String name 	= conf.getString("name");
//				String icon 	= conf.getString("icon");
//				String command 	= pcommand.getCommand();
//				String puid 	= pcommand.getUID();
//				
//				String base = "./"+md.getString("uid")+"/";
//				
//				page +=   "<li>\n"
//						+ ""
//						+ "				<div class=\"list-item-container\">"
//						+ "                    <img width='50' height='50' src='"+base+icon+"'>\n"
//						+ "\n"
//						+ "                    <div class=list-item-right>\n"
//						+ "                        <div class=\"app-title\">"+MDSHub.stripHTML(name)+"</div>\n"
//						+ ""
//						+ "                        <div><br><br><br>\n"
//						+ "                        	Command :<br><br>\n"
//						+ "                        	<table border=0 width=100%>\n"
//						+ "                        		<tr>\n"
//						+ "                        			<td style=\"font-size:12;\">"+MDSHub.stripHTML(command)+"</td>\n"
//						+ "                        		</tr>\n"
//						+ "                        	</table>\n"
//						+ "                        \n"
//						+ "                        </div>"
//						+ ""
//						+ "						   <div style=\"height:40\">\n"
//						+ "                        	<br>\n"
//						+ "                        	<table>\n"
//						+ "                        		<tr>\n"
//						+ "                        			\n"
//						+ "                        			<td>\n"
//						+ "                        				<form action=\"pendingaction.html\" method=POST onsubmit=\"return confirm('Are you sure you want to ACCEPT this action ?');\">\n"
//						+ "										  <input type='hidden' name='sessionid' value='"+zSessionID+"'>\n"
//						+ "										  <input type='hidden' name='accept' value='accept'>\n"
//						+ "										  <input type='hidden' name='uid' value='"+puid+"'>\n"
//						+ "										  <input class='acceptbutton' style='width:100;' type=\"submit\" value='&nbsp;&nbsp;Accept&nbsp;&nbsp;'>\n"
//						+ "										</form>		\n"
//						+ "                        			</td>\n"
//						+ "									<td>&nbsp;&nbsp;</td>"
//						+ "                        			<td>\n"
//						+ "                        				<form action=\"pendingaction.html\" method=POST onsubmit=\"return confirm('Are you sure you want to DENY this action ?');\">\n"
//						+ "										  <input type='hidden' name='sessionid' value='"+zSessionID+"'>\n"
//						+ "										  <input type='hidden' name='accept' value='deny'>\n"
//						+ "										  <input type='hidden' name='uid' value='"+puid+"'>\n"
//						+ "										  <input class='deletebutton' style='width:100;' type=\"submit\" value='&nbsp;&nbsp;Deny&nbsp;&nbsp;'>\n"
//						+ "										</form>		\n"
//						+ "                        			</td>\n"
//						+ "                        			\n"
//						+ "                        		</tr>\n"
//						+ "                        	</table>\n"
//						+ "                        </div>"
//						+ ""
//						+ "                    </div>\n"
//						+ "                </div>\n"
//						+ "</li>";
//			}
//		}
	
		//End the HTML
		page += MDSUtil.HUB_END;
		
		return page;
		
	}
}
