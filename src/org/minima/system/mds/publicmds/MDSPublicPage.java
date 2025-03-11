package org.minima.system.mds.publicmds;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniString;
import org.minima.system.mds.MDSManager;
import org.minima.utils.MiniFile;

public class MDSPublicPage {

	MDSManager mMDS;
	
	public ArrayList<String> PUBLIC_MINIDAPPS = new ArrayList<>();
	
	public MDSPublicPage(MDSManager zMDS) {
		mMDS = zMDS;
		
		PUBLIC_MINIDAPPS.add("docs");
		PUBLIC_MINIDAPPS.add("news feed");
		PUBLIC_MINIDAPPS.add("block");
		PUBLIC_MINIDAPPS.add("web wallet");
	}
	
	public String getIndexPage() throws IOException {
		
		//First get all the MiniDAPPs..
		ArrayList<MiniDAPP> allmini = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
		ArrayList<MiniDAPP> publicmini = new ArrayList<>();
		for(MiniDAPP mini : allmini) {
			if(PUBLIC_MINIDAPPS.contains(mini.getName().toLowerCase())) {
				publicmini.add(mini);
			}
		}
		
		//Sort alphabetically..
		Collections.sort(publicmini, new Comparator<MiniDAPP>() {
			@Override
			public int compare(MiniDAPP o1, MiniDAPP o2) {
				return o1.getName().compareTo(o2.getName());
			}			
		});
		
		//Get the base file..
		String fileRequested = loadResouceFile("publicmds/index_gen.html");
		
		//Set the Public MDS UID
		fileRequested = fileRequested.replace("###PUBLICMDSUID###",mMDS.getPublicMiniDAPPSessionID());
				
		//Now write out the list of public MiniDAPPs..
		fileRequested = fileRequested.replace("###DIVLIST###",getPublicDIVList(publicmini));
		
		return fileRequested;
	}

	public String getPublicDIVList(ArrayList<MiniDAPP> zMinis) {
		
		String divlist = "";
		
		
		for(MiniDAPP mini : zMinis) {
			
			String icon = "../"+mini.getUID()+"/"+mini.getIcon();
			
			divlist += "<div id=\"publicminidapp_"+mini.getUID()+"\">"
					+ "<button class='button-56' onclick=\"opendapp('"+mini.getUID()+"');\">"
					+ "<img width=50 src='"+icon+"'>&nbsp;&nbsp;"+mini.getName()+"</button><br>"
					+ "</div>";
		}
		
		return divlist;
	}
	
	public String loadResouceFile(String zResource) throws IOException {
		
		//Get the Resource file
		InputStream is 	= getClass().getClassLoader().getResourceAsStream(zResource);
		
		//Get all the data..
		byte[] file = MiniFile.readAllBytes(is);
		is.close();
		
		return new String(file, MiniString.MINIMA_CHARSET);
	}
}
