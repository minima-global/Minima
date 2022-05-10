package org.minima.database.maxima;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class MaximaContact {

	public String 	mUID;
	
	public String 	mName;
	
	public MiniData mExtraData;
	
	public MiniData mPublicKey;
	
	/**
	 * Where you contact them
	 */
	public String 	mCurrentHost;
	public MiniData mCurrentHostPublicKey;
	
	/**
	 * Where they contact you
	 */
	public String 	mMyCurrentHost;
	public MiniData mMyCurrentHostPublicKey;
	
	public MaximaContact(String zName, MiniData zPublicKey) {
		mName 		= zName;
		mPublicKey	= zPublicKey;
	}
	
	public void setCurrentHost(String zHost) {
		mCurrentHost = zHost;
	}
	
	public void setCurrentPublicKey(MiniData zCurrentPublicKey) {
		mCurrentHostPublicKey = zCurrentPublicKey;
	}
	
	public String getName() {
		return mName;
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public String getCurrentHost() {
		return mCurrentHost;
	}
	
	public MiniData getCurrentPublicKey() {
		return mCurrentHostPublicKey;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("name", mName);
		json.put("publickey", mPublicKey.to0xString());
		json.put("host", mCurrentHost);
		json.put("hostkey", mCurrentHostPublicKey.to0xString());
		
		return json;
	}
}
