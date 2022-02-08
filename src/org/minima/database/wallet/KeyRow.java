package org.minima.database.wallet;

import org.minima.utils.json.JSONObject;

public class KeyRow {

	public String 	mPrivateKey;
	public String 	mPubliKey;
	public String 	mScript;
	public String 	mSimpleAddress;
	public boolean  mTrack;
	
	public KeyRow(String zPrivKey, String zPubKey, String zAddress, String zScript, boolean zTrack) {
		mPrivateKey		= zPrivKey;
		mPubliKey 		= zPubKey;
		mSimpleAddress 	= zAddress;
		mScript			= zScript;
		mTrack		= zTrack;
	}
	
	public String getPrivateKey() {
		return mPrivateKey;
	}
	
	public String getPublicKey() {
		return mPubliKey;
	}
	
	public String getAddress() {
		return mSimpleAddress;
	}
	
	public String getScript() {
		return mScript;
	}
	
	public boolean trackAddress() {
		return mTrack;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("publickey", getPublicKey());
		ret.put("script", getScript());
		ret.put("address", getAddress());
		ret.put("track", mTrack);
		
		return ret;
	}
}
