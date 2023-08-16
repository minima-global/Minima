package org.minima.utils.dex;

import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class Offer {
	
	public long mTimeMilli = System.currentTimeMillis();
	
	//UID of this offer
	public String 		mUID;
	
	//Details
	public String 		mPair;
	public boolean 		mBuySell;
	public MiniNumber 	mMin;
	public MiniNumber 	mMax;
	public MiniNumber 	mPrice;
	
	//Who is it..
	public String 		mPublicKey;
	
	//How to contact them
	public String 		mMaxContact;
	
	public Offer() {}
	
	public Offer(JSONObject zJSON) {
		mUID 		= zJSON.getString("uid");
		mTimeMilli 	= Long.valueOf(zJSON.getString("time"));
		mPair		= zJSON.getString("pair");
		mBuySell 	= (boolean) zJSON.get("buysell");
		mMin		= new MiniNumber(zJSON.getString("min"));
		mMax		= new MiniNumber(zJSON.getString("max"));
		mPrice		= new MiniNumber(zJSON.getString("price"));		
		mPublicKey	= zJSON.getString("publickey");
		mMaxContact = zJSON.getString("maxcontact");
	}
	
	public JSONObject toJSON() {
		
		JSONObject ret = new JSONObject();
		
		ret.put("uid", mUID);
		ret.put("time", mTimeMilli+"");
		ret.put("pair", mPair);
		ret.put("buysell", mBuySell);
		ret.put("min", mMin.toString());
		ret.put("max", mMax.toString());
		ret.put("price", mPrice.toString());
		ret.put("publickey", mPublicKey);
		ret.put("maxcontact", mMaxContact);
		
		return ret;
	}
	
	public boolean isSameOffer(Offer zOffer) {
		return mPublicKey.equals(zOffer.mPublicKey) && 
			   mPair.equals(zOffer.mPair) &&
			   mBuySell == zOffer.mBuySell;
	}
}
