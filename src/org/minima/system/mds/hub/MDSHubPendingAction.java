package org.minima.system.mds.hub;

import org.minima.system.mds.MDSManager;

public class MDSHubPendingAction {
	
	public static String createHubPage(MDSManager zMDS, String zSessionID, boolean zAccept, String zOuput) {
		
		//Start the HTML
		String page = MDSUtil.HUB_START;
		
		//First the back button..
		page += MDSUtil.returnHeader(zSessionID, true, false);
		
		//Now are we accepting
		if(zAccept) {
			page+="<center><div class='app-title'>Pending action ACCEPTED!..</div></center>\n";
			
		}else {
			page+="<center><div class='app-title'>Pending action DENIED!..</div></center>\n";
		}
		
		//And now add the output
		page+=    "<div class='list-container'>\r\n"
				+ "	<div class='list-item-container' style=\"text-align: left;overflow-wrap: break-word;display: inline-block;\">\r\n"
				+ "		<pre id=\"json\">"+MDSHub.stripHTML(zOuput)+"</pre>\r\n"
				+ "	</div>\r\n"
				+ "</div>";
		
		//Now the return form..
		page +=  "\n"
				+ "			</ul>"
				+ "		</div>\n"
				+ "<script type=\"text/javascript\">\n"
				+ "\n"
				+ "(function() {\n"
				+ "    var element = document.getElementById(\"json\");\n"
				+ "    var obj = JSON.parse(element.innerText);\n"
				+ "    element.innerHTML = JSON.stringify(obj, undefined, 2);\n"
				+ "})();\n"
				+ "\n"
				+ "</script>"
				+ ""
				+ "</html>";
		
		return page;
		
	}
}
