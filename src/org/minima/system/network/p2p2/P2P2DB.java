package org.minima.system.network.p2p2;

import java.io.File;
import java.util.ArrayList;

import org.minima.utils.JsonDB;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;

public class P2P2DB extends JsonDB {

	ArrayList<String> mAllPeers = new ArrayList<>();
	
	public P2P2DB() {
		super();
	}
	
	@Override
	public void loadDB(File zFile) {
		//Load the file.. and convert the Peers List
		MiniFile.loadObjectSlow(zFile, this);
		
		mAllPeers.clear();
		
		//Now cycle and add the peers to our internal list
		JSONArray allpeers = getJSONArray("all_known_peers");
		for(Object peer : allpeers) {
			String strpeer = (String)peer;
			mAllPeers.add(strpeer);
		}
	}
	
	@Override
	public void saveDB(File zFile) {
		
		//First convert the peers list..
		JSONArray peerslist = new JSONArray();
		for(String peer : mAllPeers) {
			peerslist.add(peer);
		}
		
		//Now set it..
		setJSONArray("all_known_peers", peerslist);
		
		//And NOW save it..
		MiniFile.saveObjectDirect(zFile, this);
	}
	
	public boolean isFirstStartUp() {
		return getBoolean("first_startup", true);
	}
	
	public void setFirstStartUp(boolean zSet) {
		setBoolean("first_startup", zSet);
	}
	
	public ArrayList<String> getAllKnownPeers(){
		return mAllPeers;
	}
	
	public boolean addPeerToAllKnown(String zPeer){
		if(!mAllPeers.contains(zPeer)) {
			mAllPeers.add(zPeer);
			MinimaLogger.log("Peer added : "+zPeer);
			return true;
		}
		
		MinimaLogger.log("Peer already added : "+zPeer);
		return false;
	}
	
	
}
