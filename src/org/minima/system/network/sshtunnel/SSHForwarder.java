package org.minima.system.network.sshtunnel;

import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

public class SSHForwarder implements Runnable {

	JSch mSSH = null;
	
	Session mSession = null;
	
	String mHost;
	int mPort;
	
	String mUsername;
	String mPassword;
	boolean mIsPublicKey;
	
	int mRemotePort;
	
	boolean mRunning = false;
	
	public SSHForwarder(String zHost, int zPort, String zUsername, String zPassword, boolean zIsPublicKey, int zRemotePortForward) {
		mHost = zHost;
		mPort = zPort;
		mUsername = zUsername;
		mPassword = zPassword;
		mIsPublicKey = zIsPublicKey;
		mRemotePort = zRemotePortForward;
	}
	
	public boolean isRunning() {
		return mRunning;
	}
	
	public boolean isConnected() {
		if(!mRunning || mSession == null) {
			return false;
		}
		
		return mSession.isConnected();
	}
	
	public void stop() {
		if(!mRunning) {
			return;
		}
		
		mRunning = false;
		
		if(mSession != null) {
			
			try {
				//Stop port forwarding
				mSession.delPortForwardingR(mRemotePort);
			} catch (JSchException e) {
				MinimaLogger.log(e);
			}
			
			//Shutdown..
			mSession.disconnect();
		}
	}
	
	@Override
	public void run() {
		mRunning = true;
		
//		//Base Object
//		mSSH = new JSch();
//		
//		//Are we using a Private key
//		if(mIsPublicKey) {
//			//Add the Private Key..
//			try {
//				mSSH.addIdentity(mPassword);
//			} catch (JSchException e) {
//				MinimaLogger.log(e);
//				return;
//			}
//		}
//		
//		try {
//			//Get the session..
//			mSession = mSSH.getSession(mUsername, mHost, mPort);
//			mSession.setConfig("StrictHostKeyChecking", "no");
//			
//			//Is this a User name and Password or a Private  Key..
//			if(!mIsPublicKey) {
//				mSession.setPassword(mPassword);
//			}
//		
//		} catch (JSchException e) {
//			MinimaLogger.log(e);
//			return;
//		}
		
		//What are thge current details
		boolean isaccepting 	= GeneralParams.IS_ACCEPTING_IN_LINKS;
		boolean ishostset 		= GeneralParams.IS_HOST_SET;
		String chost			= GeneralParams.MINIMA_HOST;
		int cport 				= GeneralParams.MINIMA_PORT;
		
		//Now stay connected..
		while(isRunning()) {
		    try {
		    	
		    	//Are we already running..
		    	if(mSession != null) {
		    		MinimaLogger.log("Shutting down running JSCH session..");
		    		
	    			//Stop port forwarding
					try {mSession.delPortForwardingR(mRemotePort);}catch(JSchException exc) {}
		    		
					//Shutdown..
					mSession.disconnect();
					mSession = null;
					
					//Small pause
					Thread.sleep(1000);
		    	}
		    	
	    		//Base Object
	    		mSSH = new JSch();
	    		
	    		//Are we using a Private key
	    		if(mIsPublicKey) {
	    			//Add the Private Key..
	    			mSSH.addIdentity(mPassword);
	    		}
	    		
    			//Get the session..
    			mSession = mSSH.getSession(mUsername, mHost, mPort);
    			mSession.setConfig("StrictHostKeyChecking", "no");
    			
    			//Is this a User name and Password or a Private  Key..
    			if(!mIsPublicKey) {
    				mSession.setPassword(mPassword);
    			}
	    		
		    	//Now connect
		    	mSession.connect(30000);
		    	
		    	//30 second keep alive..
		    	mSession.setServerAliveInterval(30000);
		    	
		    	//Stop port forwarding
				try {mSession.delPortForwardingR("*",mRemotePort);}catch(JSchException exc) {
					MinimaLogger.log("DELR ERROR");
					MinimaLogger.log(exc);
				}
				
		    	//Port forward - Minima
		    	mSession.setPortForwardingR("*",mRemotePort, "127.0.0.1", cport);
		    	
		    	//Log it..
		    	MinimaLogger.log("SSH Tunnel STARTED Minima @ "+mHost+":"+mRemotePort+" to "+cport);
		    	
		    	//Set the GeneralParams..
		    	GeneralParams.IS_ACCEPTING_IN_LINKS 	= true;
		    	GeneralParams.IS_HOST_SET 				= true;
				GeneralParams.MINIMA_HOST 				= mHost;
				GeneralParams.MINIMA_PORT 				= mRemotePort;
				
				//Now make sure we are connected..
		    	while(mSession.isConnected()) {
		    		Thread.sleep(1000);
		    	}
		    	
		    	if(isRunning()) {
			    	MinimaLogger.log("SSH Tunnel connection lost.. reconnecting in 10s");
			    	Thread.sleep(10000);
		    	}
		    	
	    	}catch(Exception ex) {
		       MinimaLogger.log(ex);
		       
		       try {Thread.sleep(10000);} catch (InterruptedException e) {}
		    }
		}
		
		//Nullify the Session
		mSession = null;
		
		//Reset HOST / PORT values
		GeneralParams.IS_ACCEPTING_IN_LINKS = isaccepting;
		GeneralParams.IS_HOST_SET 			= ishostset;
		GeneralParams.MINIMA_HOST 			= chost;
		GeneralParams.MINIMA_PORT 			= cport;
		
		//Tell the User
		MinimaLogger.log("SSH Tunnel STOPPED");
	}

}