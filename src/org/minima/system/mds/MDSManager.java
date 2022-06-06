package org.minima.system.mds;

import java.io.File;
import java.net.Socket;

import org.minima.system.network.rpc.MDSFileHandler;
import org.minima.system.mds.polling.PollHandler;
import org.minima.system.mds.polling.PollStack;
import org.minima.system.network.rpc.HTTPServer;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.Stack;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MDSManager extends MessageProcessor {

	public static final String MDS_INIT 		= "MDS_INIT";
	public static final String MDS_POLLMESSAGE 	= "MDS_POLLMESSAGE";
	
	HTTPServer mMDSServer;
	HTTPServer mPollServer;
	
	File mMDSRootFile; 
	
	PollStack mPollStack;
	
	public MDSManager() {
		super("MDS");
		
		mPollStack = new PollStack();
		
		PostMessage(MDS_INIT);
	}
	
	public void shutdown() {
		
		//Shut down the server
		mMDSServer.stop();
		mPollServer.stop();
		
		stopMessageProcessor();
	}
	
	public File getRootFolder() {
		return mMDSRootFile;
	}
	
	public File getWebFolder(String zUID) {
		File web 	= new File(mMDSRootFile, "web");
		File dapp 	= new File(web, zUID);
		return dapp;
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
					return new MDSFileHandler( new File(mMDSRootFile,"web") , zSocket);
				}
			};
			
			//The Polling Server
			mPollServer = new HTTPServer(9004) {
				@Override
				public Runnable getSocketHandler(Socket zSocket) {
					return new PollHandler(mPollStack, zSocket);
				}
			};
		
		}else if(zMessage.getMessageType().equals(MDS_POLLMESSAGE)) {
			
			// Add a message to the POll..
			JSONObject poll = (JSONObject) zMessage.getObject("poll");
			
			//Add to the Poll Stack
			mPollStack.addMessage(poll);
		}
		
	}

}
