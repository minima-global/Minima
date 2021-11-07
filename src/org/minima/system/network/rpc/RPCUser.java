package org.minima.system.network.rpc;

import org.minima.utils.json.JSONObject;

public class RPCUser {

	/**
	 * The Types of Access and RPC User can have
	 */
	public static final int RPC_READ 	= 0;
	public static final int RPC_WRITE 	= 1;
	
	public String mUsername;
	public String mPassword;
	public int zAccessType;
	
	public RPCUser() {
		
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		
		
		return ret;
	}
}
