package org.minima.utils.dex;

import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class DexManager {

	DexData mDexData;
	
	public DexManager() {
		mDexData = new DexData();
	}
	
	public void init() {
		
		
		
	}
	
	/**
	 * Check Public keys, details, allowed lists etc..
	 */
	public boolean checkOffer(Offer zOffer) {
		return true;
	}
	
	public synchronized JSONObject processRequest(String zRequest, String zData) throws Exception {
		
		JSONObject ret = new JSONObject();
		ret.put("status", true);
		
		if(zRequest.equals("get")) {
			
			//Add all the offers
			ret.put("offers", mDexData.getAllOffers());
			
			return ret;
			
		}else if(zRequest.equals("set")) {
			
			//Convert data to JSON
			JSONObject json =  (JSONObject) new JSONParser().parse(zData);
			
			//Convert to an offer object
			Offer offer = new Offer(json);
			
			//Check the request Data
			if(!checkOffer(offer)) {
				ret = new JSONObject();
				ret.put("status", false);
				ret.put("error", "Invalid offer : ");
				return ret;
			}
			
			//Now set this offer..
			mDexData.updateOffer(offer);
			
			//And return true
			ret.put("comment", "Offer updated");
			
			return ret;
		}
		
		ret = new JSONObject();
		ret.put("status", false);
		ret.put("error", "Undefined command : "+zRequest);
		
		return ret;
	}
}
