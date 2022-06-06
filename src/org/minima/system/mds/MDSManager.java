package org.minima.system.mds;

import java.io.File;
import java.net.Socket;

import org.minima.system.network.rpc.MDSFileHandler;
import org.minima.system.network.rpc.HTTPServer;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MDSManager extends MessageProcessor {

	public static final String MDS_INIT 		= "MDS_INIT";
	
	public static final String MDS_INSTALL 		= "MDS_INSTALL";
	public static final String MDS_UNINSTALL 	= "MDS_UNINSTALL";
	
	public static final String MDS_REFRESH 		= "MDS_REFRESH";
	
	HTTPServer mMDSServer;
	
	File mMDSRootFile; 
	
	public MDSManager() {
		super("MDS");
		
		PostMessage(MDS_INIT);
	}
	
	public void shutdown() {
		
		//Shut down the server
		mMDSServer.stop();
		
		stopMessageProcessor();
	}
	
	public JSONObject getMDSList() {		
		JSONObject ret = new JSONObject();
		
		
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(MDS_INIT)) {
			
			//What is the root folder
			mMDSRootFile = new File(GeneralParams.DATA_FOLDER,"mds");
			
			//Create a new Server
			mMDSServer = new HTTPServer(9003) {
				
				@Override
				public Runnable getSocketHandler(Socket zSocket) {
					return new MDSFileHandler(mMDSRootFile, zSocket);
				}
			};
		
		}else if(zMessage.getMessageType().equals(MDS_REFRESH)) {
			
			//Load the MiniDAPPs..
			File webfolder = new File(mMDSRootFile,"web");
			
			//List the folders
			File[] files = webfolder.listFiles();
			if(files != null) {
				for(int i=0;i<files.length;i++) {

					if(files[i].isDirectory()) {
						
						
						
						
					}
					
				}
			}
			
			
		}else if(zMessage.getMessageType().equals(MDS_INSTALL)) {
		
			//Load the file..
			
		}else if(zMessage.getMessageType().equals(MDS_UNINSTALL)) {
			
			
		}
		
	}

}
