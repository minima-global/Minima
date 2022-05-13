package org.minima.database.wallet;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.minima.utils.json.JSONObject;

public class KeyRow {

	public int mSize;
	public int mDepth;
	
	public int mUses;
	public int mMaxUses;
	
	public String 	mModifier;
	
	public String 	mPublicKey;
	public String 	mPrivateKey;
	
	public KeyRow(ResultSet zResults) throws SQLException {
		mSize 		= zResults.getInt("size");
		mDepth 		= zResults.getInt("depth");
		mUses 		= zResults.getInt("uses");
		mMaxUses 	= zResults.getInt("maxuses");
		mModifier 	= zResults.getString("modifier");
		mPrivateKey = zResults.getString("privatekey");
		mPublicKey 	= zResults.getString("publickey");
	}
	
	public KeyRow(int zSize, int zDepth, int zUses, int zMaxUses, String zModifier, String zPrivate, String zPublic) {
		mSize 		= zSize;
		mDepth 		= zDepth;
		mUses 		= zUses;
		mMaxUses 	= zMaxUses;
		mModifier 	= zModifier;
		mPrivateKey = zPrivate;
		mPublicKey 	= zPublic;
	}
	
	public int getSize() {
		return mSize;
	}
	
	public int getDepth() {
		return mDepth;
	}
	
	public int getUses() {
		return mUses;
	}
	
	public int getMaxUses() {
		return mMaxUses;
	}
	
	public String getModifier() {
		return mModifier;
	}
	
	public String getPrivateKey() {
		return mPrivateKey;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("size", mSize);
		ret.put("depth", mDepth);
		ret.put("uses", mUses);
		ret.put("maxuses", mMaxUses);
		ret.put("modifier", getModifier());
//		ret.put("privatekey", getPrivateKey());
		ret.put("publickey", getPublicKey());
		
		return ret;
	}
}
