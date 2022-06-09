package org.minima.system.mds;

import java.io.File;
import java.net.Socket;

import org.minima.system.mds.polling.PollHandler;
import org.minima.system.mds.polling.PollStack;
import org.minima.system.network.rpc.HTTPServer;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
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
		
		if(!GeneralParams.MDS_ENABLED) {
			MinimaLogger.log("MDS disabled");
			return;
		}
		
		PostMessage(MDS_INIT);
	}
	
	public void shutdown() {
		
		//Shut down the server
		if(GeneralParams.MDS_ENABLED) {
			mMDSServer.stop();
			mPollServer.stop();
		}
		
		stopMessageProcessor();
	}
	
	public File getRootFolder() {
		return mMDSRootFile;
	}
	
	public File getWebFolder() {
		return new File(mMDSRootFile, "web");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Is it even enabled
		if(!GeneralParams.MDS_ENABLED) {
			return;
		}
		
		if(zMessage.getMessageType().equals(MDS_INIT)) {
			
			//What is the root folder
			mMDSRootFile = new File(GeneralParams.DATA_FOLDER,"mds");
			
			//Create a new Server
			mMDSServer = new HTTPServer(GeneralParams.MDS_PORT) {
				
				@Override
				public Runnable getSocketHandler(Socket zSocket) {
					return new MDSFileHandler( new File(mMDSRootFile,"web") , zSocket);
				}
			};
			
			//The Polling Server
			mPollServer = new HTTPServer(GeneralParams.POLL_PORT) {
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
