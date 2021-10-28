package org.minima.system.network.minima;

import java.util.Date;

import org.minima.utils.json.JSONObject;

public class NIOClientInfo {

	/**
	 * The MAin NIOClient
	 */
	NIOClient 	mNIOClient;
	
	/**
	 * Are we connected yet
	 */
	boolean 	mConnected;
	
	public NIOClientInfo(NIOClient zNIOClient, boolean zConnected) {
		mNIOClient 	= zNIOClient;
		mConnected	= zConnected;
	}
	
	public String getUID() {
		return mNIOClient.getUID();
	}
	
	public boolean isIncoming() {
		return mNIOClient.isIncoming();
	}
	
	public boolean isConnected() {
		return mConnected;
	}
	
	public String getHost() {
		return mNIOClient.getHost();
	}
	
	public int getPort() {
		return mNIOClient.getPort();
	}
	
	public long getTimeConnected() {
		return mNIOClient.getTimeConnected();
	}
	
	public Object getExtraData() {
		return mNIOClient.getExtraData();
	}
	
	public void setExtrasData(Object zExtraData) {
		mNIOClient.setExtraData(zExtraData);
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("uid", getUID());
		ret.put("incoming", isIncoming());
		ret.put("host", getHost());
		ret.put("port", getPort());
		ret.put("isconnected", isConnected());
		ret.put("connected", new Date(getTimeConnected()).toString());
		
		return ret;
	}
}
