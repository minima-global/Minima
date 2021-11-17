package org.minima.system.network.sshtunnel;

import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

public class SSHForwarder implements Runnable {

	JSch mSSH;
	
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
		
		//Base Object
		mSSH = new JSch();
		
		//Are we using a Private key
		if(mIsPublicKey) {
			//Add the Private Key..
			try {
				mSSH.addIdentity(mPassword);
			} catch (JSchException e) {
				MinimaLogger.log(e);
				return;
			}
		}
		
		try {
			//Get the session..
			mSession = mSSH.getSession(mUsername, mHost, mPort);
			mSession.setConfig("StrictHostKeyChecking", "no");
			
			//Is this a User name and Password or a Private  Key..
			if(!mIsPublicKey) {
				mSession.setPassword(mPassword);
			}
		
		} catch (JSchException e) {
			MinimaLogger.log(e);
			return;
		}
		
		//Now stay connected..
		while(isRunning()) {
		    try {
		    	//Connect!..with tmeout
		    	mSession.connect(30000);
		       
		    	//Port forward - Minima
		    	mSession.setPortForwardingR("*",mRemotePort, "127.0.0.1", GeneralParams.MINIMA_PORT);
		    	
		    	//Log it..
		    	MinimaLogger.log("SSH Tunnel STARTED Minima @ "+mHost+":"+mRemotePort+" to "+GeneralParams.MINIMA_PORT);
		    	
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
		
		MinimaLogger.log("SSH Tunnel STOPPED");
	}

}