package org.minima.database.wallet;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class ScriptRow {

	public String 	mScript;
	public String 	mAddress;
	public boolean  mTrack;
	
	public boolean  mSimple;
	public String  	mPublicKey;
	
	public ScriptRow() {
//		mPrivateKey		= zPrivKey;
//		mPubliKey 		= zPubKey;
//		mSimpleAddress 	= zAddress;
//		mScript			= zScript;
//		mTrack			= zTrack;
	}
	
//	public String getPrivateKey() {
//		return mPrivateKey;
//	}
//	
//	public String getPublicKey() {
//		return mPubliKey;
//	}
//	
//	public String getAddress() {
//		return mSimpleAddress;
//	}
	
	public String getScript() {
		return mScript;
	}
	
	public boolean trackAddress() {
		return mTrack;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
//		ret.put("publickey", getPublicKey());
//		ret.put("script", getScript());
//		ret.put("address", getAddress());
//		ret.put("miniaddress", Address.makeMinimaAddress(new MiniData(getAddress())));
//		ret.put("track", mTrack);
		
		return ret;
	}
}
