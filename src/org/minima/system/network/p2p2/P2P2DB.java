package org.minima.system.network.p2p2;

import org.minima.utils.JsonDB;
import org.minima.utils.json.JSONArray;

public class P2P2DB extends JsonDB {

	public P2P2DB() {
		super();
	}
	
	public JSONArray getAllKnownPeers(){
		
		JSONArray allpeers = new JSONArray();
		allpeers.add("127.0.0.1:10001");
		
		return allpeers;
	}
	
	
}
