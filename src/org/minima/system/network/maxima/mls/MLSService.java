package org.minima.system.network.maxima.mls;

import java.util.Hashtable;

import org.minima.utils.MinimaLogger;

public class MLSService {

	long mMLSSetTime		= 0;
	String mMLSServer 		= "";
	
	String mOldMLSServer 	= "";
	
	//Where we store all the info..
	Hashtable<String, MLSPacketSET> mCurrentMLS = new Hashtable<>();
	
	public MLSService() {}
	
	public void hardSetMLSNode(String zOldMLSNode, String zMLSNode, long zMilliTime) {
		mOldMLSServer 	= zOldMLSNode;
		mMLSServer 		= zMLSNode;
		mMLSSetTime 	= zMilliTime;
	}
	
	public boolean newMLSNode(String zMLSNode) {
		
		//Do we have one already..
		if(mMLSServer.equals("")) {
			mMLSSetTime 	= System.currentTimeMillis();
			mMLSServer  	= zMLSNode;
		
			MinimaLogger.log("Setting MLS server : "+mMLSServer);
			
			return true;
			
		}else {
			
			//Is it time for a new one..
			
			
		}
		
		return false;
	}
	
	public String getMLSServer() {
		return mMLSServer;
	}
	
	public long getMLSTime() {
		return mMLSSetTime;
	}
	
	public String getOldMLSServer() {
		return mOldMLSServer;
	}
	
	public void addMLSData(String zPublicKey, MLSPacketSET zData) {
		mCurrentMLS.put(zPublicKey, zData);
	}
	
	public MLSPacketSET getData(String zPublicKey) {
		return mCurrentMLS.get(zPublicKey);
	}
}
