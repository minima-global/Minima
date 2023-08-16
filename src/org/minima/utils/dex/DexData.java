package org.minima.utils.dex;

import java.util.ArrayList;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class DexData {

	public ArrayList<Offer> mAllOffers;
	
	public DexData() {
		mAllOffers = new ArrayList<>();
	}
	
	public JSONArray getAllOffers() {
		
		JSONArray ret = new JSONArray();
		for(Offer offer :mAllOffers) {
			ret.add(offer.toJSON());
		}
		
		return ret;
	}
	
	public void updateOffer(Offer zOffer) {
		
		ArrayList<Offer> newOffers = new ArrayList<>();
		for(Offer offer :mAllOffers) {
			//Check if this is the offer we are replacing..
			if(!offer.isSameOffer(zOffer)) {
				newOffers.add(offer);
			}
		}
		
		//Now add this offer..
		newOffers.add(zOffer);
		
		//And now make these the offers
		mAllOffers = newOffers;
	}
}
