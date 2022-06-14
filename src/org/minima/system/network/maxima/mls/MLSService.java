package org.minima.system.network.maxima.mls;

import java.util.Enumeration;
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
	
	//Remove OLD data
	public void flushList() {
		Hashtable<String, MLSPacketSET> newMLS = new Hashtable<>();
		
		//24 hours old get removed
		long mintime = System.currentTimeMillis() - (1000 * 60 * 60 * 24); 
		
		Enumeration<String> keys = mCurrentMLS.keys();
		while(keys.hasMoreElements()) {
			
			String key 			= keys.nextElement(); 
			MLSPacketSET mls 	= mCurrentMLS.get(key);
			if(mls.getMilliTime() > mintime) {
				newMLS.put(key, mls);
			}
		}
		
		//Switch
		mCurrentMLS = newMLS;
	}
	
	public boolean newMLSNode(String zMLSNode) {
		
		//Do we have one already..
		if(mMLSServer.equals("")) {
			mMLSSetTime 	= System.currentTimeMillis();
			mMLSServer  	= zMLSNode;
			mOldMLSServer	= "";
			
			MinimaLogger.log("Setting MLS server : "+mMLSServer);
			
			return true;
			
		}else {
			
			//Is it time for a new one.. every 12 hours
			long timediff = System.currentTimeMillis() - mMLSSetTime;
			if(timediff > 1000 * 60 * 60 * 12) {
			
				//Cycle.. current is Old
				mOldMLSServer = mMLSServer;
				
				//And store the new..
				mMLSSetTime 	= System.currentTimeMillis();
				mMLSServer  	= zMLSNode;
				
				MinimaLogger.log("Setting MLS server : "+mMLSServer);
				
				return true;
			}
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
