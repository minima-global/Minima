package org.minima.database.wallet;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class ScriptRow {

	public String 	mScript;
	public String 	mAddress;
	
	public boolean  mSimple;
	
	public boolean  mDefault;
	
	public String  	mPublicKey;
	
	public boolean  mTrack;
	
	public ScriptRow(ResultSet zResults) throws SQLException {
		mScript 	= zResults.getString("script");
		mAddress 	= zResults.getString("address");
		
		int simple  = zResults.getInt("simple");
		if(simple == 0) {
			mSimple = false;
		}else {
			mSimple = true;
		}
		
		int def  = zResults.getInt("defaultaddress");
		if(def == 0) {
			mDefault = false;
		}else {
			mDefault = true;
		}
		
		mPublicKey 	= zResults.getString("publickey");
		
		int track  = zResults.getInt("track");
		if(track == 0) {
			mTrack = false;
		}else {
			mTrack = true;
		}
	}
	
	public ScriptRow(String zScript, String zAddress, boolean zSimple, boolean zDefault, String zPublicKey, boolean zTrack) {
		mScript 	= zScript;
		mAddress 	= zAddress;
		mSimple 	= zSimple;
		mDefault	= zDefault;
		mPublicKey 	= zPublicKey;
		mTrack		= zTrack;
	}
	
	public String getScript() {
		return mScript;
	}
	
	public String getAddress() {
		return mAddress;
	}
	
	public boolean isSimple() {
		return mSimple;
	}
	
	public boolean isDefault() {
		return mDefault;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public boolean isTrack() {
		return mTrack;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("script", getScript());
		ret.put("address", getAddress());
		ret.put("miniaddress", Address.makeMinimaAddress(new MiniData(getAddress())));
		ret.put("simple", isSimple());
		ret.put("default", isDefault());
		ret.put("publickey", getPublicKey());
		ret.put("track", isTrack());
		
		return ret;
	}
}
