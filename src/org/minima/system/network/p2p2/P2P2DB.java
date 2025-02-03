package org.minima.system.network.p2p2;

import org.minima.utils.JsonDB;
import org.minima.utils.json.JSONArray;

public class P2P2DB extends JsonDB {

	public P2P2DB() {
		super();
	}
	
	public boolean isFirstStartUp() {
		return getBoolean("first_startup", true);
	}
	
	public void setFirstStartUp(boolean zSet) {
		setBoolean("first_startup", zSet);
	}
	
	
	
	public void clearAllKnownPeers() {
		setJSONArray("all_known_peers", new JSONArray());
	}
	
	public JSONArray getAllKnownPeers(){
		return getJSONArray("all_known_peers");
	}
	
	public JSONArray addPeerToAllKnown(){
		
		return getJSONArray("all_known_peers");
	}
	
	
}
