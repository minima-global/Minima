package org.minima.system.network.p2p2;

import java.util.ArrayList;

import org.minima.utils.JsonDB;

public class P2P2DB extends JsonDB {

	public P2P2DB() {
		super();
	}
	
	public ArrayList<String> getAllKnownPeers(){
		
		ArrayList<String> allpeers = new ArrayList<>();
		allpeers.add("127.0.0.1:10001");
		
		return allpeers;
	}
	
	
}
