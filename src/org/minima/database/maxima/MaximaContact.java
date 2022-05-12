package org.minima.database.maxima;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class MaximaContact {

	public int 		mUID;
	
	public String 	mName;
	
	public MiniData mExtraData;
	
	/**
	 * The actual MAIN public Key of the Contact
	 */
	public String mPublicKey;
	
	/**
	 * Where you contact them
	 */
	public String 	mCurrentAddress;
	
	/**
	 * Where they contact you
	 */
	public String 	mMyCurrentAddress;
	
	public MaximaContact(String zName, String zPublicKey) {
		mName 		= zName;
		mPublicKey	= zPublicKey;
	}
	
	public MaximaContact(ResultSet zSQLResult) throws SQLException {
		mUID			= zSQLResult.getInt("id");
		mName			= zSQLResult.getString("name");
		mExtraData		= new MiniData(zSQLResult.getBytes("extradata"));
		mPublicKey		= zSQLResult.getString("publickey");
		mCurrentAddress	= zSQLResult.getString("currentaddress");
		mMyCurrentAddress	= zSQLResult.getString("myaddress");
	}
	
	public void setExtraData(MiniData zExtra){
		mExtraData = zExtra;
	}
	
	public void setCurrentAddress(String zAddress) {
		mCurrentAddress = zAddress;
	}
	
	public void setMyAddress(String zMyAddress) {
		mMyCurrentAddress = zMyAddress;
	}
	
	public int getUID() {
		return mUID;
	}
	
	public String getName() {
		return mName;
	}
	
	public MiniData getExtraData() {
		return mExtraData;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public String getCurrentAddress() {
		return mCurrentAddress;
	}
	
	public String getMyAddress() {
		return mMyCurrentAddress;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("id", mUID);
		json.put("name", mName);
		json.put("publickey", mPublicKey);
		json.put("currentaddress", mCurrentAddress);
		json.put("myaddress", mMyCurrentAddress);
		
		return json;
	}
}
