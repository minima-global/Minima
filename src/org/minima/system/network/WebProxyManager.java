package org.minima.system.network;

import java.io.PrintWriter;
import java.net.Socket;
import java.util.Random;

import org.minima.system.input.InputHandler;
import org.minima.system.input.InputMessage;
import org.minima.utils.ResponseStream;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class WebProxyManager extends MessageProcessor {

	public static final String WEBPROXY_MESSAGE_END = "!$&$!";
	
	/**
	 * WEBPROXY Messages
	 */
	public static final String WEBPROXY_STARTUP 		= "WEBPROXY_STARTUP";
	public static final String WEBPROXY_SHUTDOWN 		= "WEBPROXY_SHUTDOWN";
	
	public static final String WEBPROXY_REQUEST 	    = "WEBPROXY_REQUEST";
	public static final String WEBPROXY_NOTIFY 	     	= "WEBPROXY_NOTIFY";
	
	private static final String WEBPROXY_TIMEOUT 	    = "WEBPROXY_TIMEOUT";
	
	//Main Network Handler
	NetworkHandler mNetworkMain;
	InputHandler mInputHandler;
	
	//The socket
	Socket mSocket;
	
	//Output streams
	PrintWriter mOutput;
	
	Thread 				mInputThread;
	WebProxyReader		mProxyReader;
	
	//The UID
	String mUID;
	
	//The Host and Port
	String mHost;
	int    mPort;
	
	String mWebHostID;
	
	boolean mRunning = false;
	
	/**
	 * If not used will shut down
	 */
	long mLastRequest;
	
	
	public WebProxyManager(String zHost, int zPort, String zWebHostID, NetworkHandler zNetwork) {
		super("NETCLIENT");
		
		//Store
		mHost = zHost;
		mPort = zPort;
		mWebHostID = zWebHostID;
		
		mNetworkMain  = zNetwork;
		mInputHandler = zNetwork.getMainHandler().getInputHandler();
		
		//Create a UID
		mUID = ""+Math.abs(new Random().nextInt());
				
		//Store
		try {
			mSocket = new Socket(zHost, zPort);
			mRunning = true;
			
		}catch (Exception e) {
			e.printStackTrace();
			return;
		}	
		
		//Start the system..
		PostMessage(WEBPROXY_STARTUP);
	}
	
	public boolean isRunning() {
		return mRunning;
	}
	
	public Socket getSocket() {
		return mSocket;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.isMessageType(WEBPROXY_STARTUP)) {
			//Create the streams on this thread
			mOutput 	= new PrintWriter(mSocket.getOutputStream());
			
			//Start reading
			mProxyReader = new WebProxyReader(this);
			mInputThread = new Thread(mProxyReader);
			mInputThread.start();
			
			//Store this..
			mLastRequest = System.currentTimeMillis();
			
			//Write out the WebHost we are trying to conmnect to
			pushMessage("init:"+mWebHostID);
			
			//Start a timer..
			PostTimerMessage(new TimerMessage(60000, WEBPROXY_TIMEOUT));
			
		}else if(zMessage.isMessageType(WEBPROXY_TIMEOUT)) {
			//Check the time delay since it was last used
			long timenow = System.currentTimeMillis();
			long timediff = timenow - mLastRequest;
			
			//10 minutes allowance
			if(timediff > 600000) {
				//Should shut down..
				//..
			}else {
				//Re time..
				PostTimerMessage(new TimerMessage(60000, WEBPROXY_TIMEOUT));
			}
			
		}else if(zMessage.isMessageType(WEBPROXY_SHUTDOWN)) {
			mRunning = false;
			
			try {mOutput.close();}catch(Exception exc) {}
			try {mSocket.close();}catch(Exception exc) {}
			
			try {mInputThread.interrupt();}catch(Exception exc) {}
			
			stopMessageProcessor();
		
		}else if(zMessage.isMessageType(WEBPROXY_NOTIFY)) {
			//Notify all the network users that are proxying to you..
			pushMessage("notify:change");
			
		}else if(zMessage.isMessageType(WEBPROXY_REQUEST)) {
			//Store this..
			mLastRequest = System.currentTimeMillis();
			
			//Need to run a function and return the result..
			String function = zMessage.getString("function");
			String rid      = zMessage.getString("randid");
			
			//Make a response object
			ResponseStream response = new ResponseStream();
			
			//Create an input message
			InputMessage inmsg = new InputMessage(function, response);
            
			//Now post it..
			mInputHandler.PostMessage(inmsg);

			//Wait for the function to finish
            response.waitToFinish();
            
            //Get the response..
            String resp = response.getResponse();
            
			//And send it BACK!
            pushMessage(rid+":"+resp);
		}
	}
	
	private void pushMessage(String zMessage) {
		mOutput.println(zMessage+WEBPROXY_MESSAGE_END);
        mOutput.flush();
	}
}
