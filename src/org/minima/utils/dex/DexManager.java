package org.minima.utils.dex;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

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
	public boolean checkOffer(JSONObject zOffer) {
		
		//Get the offer and signature
		JSONObject jsonoffer 	= (JSONObject) zOffer.get("offer"); 
		String signature		= zOffer.getString("signature");
		
		//Check the data is signed correctly..
		String jsonstr 		= jsonoffer.toString();
		MiniData hexdata 	= new MiniData(new MiniString(jsonstr).getData());
		
		MiniData pubkey 	= new MiniData(jsonoffer.getString("publickey"));
		MiniData sig		= new MiniData(signature);
		
		boolean valid = false;
		try {
			valid = SignVerify.verify(pubkey.getBytes(), hexdata.getBytes(), sig.getBytes());
		} catch (Exception e) {
			return false;
		}
		
		//IS it valid
		if(!valid) {
			return false;
		}
		
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

			//Check the request Data
			if(!checkOffer(json)) {
				ret = new JSONObject();
				ret.put("status", false);
				ret.put("error", "Invalid offer!");
				return ret;
			}
			
			//Get the offer and signature
			JSONObject jsonoffer 	= (JSONObject) json.get("offer"); 
			String signature		= json.getString("signature");
			
			//Add some vars!
			jsonoffer.put("uid", MiniData.getRandomData(32).to0xString());
			jsonoffer.put("time", System.currentTimeMillis()+"");
			
			//Convert to an offer object
			Offer offer = new Offer(jsonoffer);
						
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
