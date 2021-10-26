package org.minima.database.wallet;

import org.minima.utils.json.JSONObject;

public class KeyRow {

	public String 	mPrivateKey;
	public String 	mPubliKey;
	public String 	mScript;
	public String 	mSimpleAddress;
	
	public KeyRow(String zPrivKey, String zPubKey, String zAddress, String zScript) {
		mPrivateKey		= zPrivKey;
		mPubliKey 		= zPubKey;
		mSimpleAddress 	= zAddress;
		mScript			= zScript;
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
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("publickey", getPublicKey());
		ret.put("script", getScript());
		ret.put("address", getAddress());
		
		return ret;
	}
}
