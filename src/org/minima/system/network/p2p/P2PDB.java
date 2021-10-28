package org.minima.system.network.p2p;

import org.minima.system.params.GlobalParams;
import org.minima.utils.JsonDB;

public class P2PDB extends JsonDB {

	public P2PDB() {
		super();
	}
	
	public void setSomething(String zHello) {
		setString("hello", zHello);
	}
	
	/**
	 * Set a default string !
	 * 
	 * Here if hello is not set will return - P2P Here!
	 */
	public String getSomething() {
		return getString("hello", "P2P Here!");
	}
}
