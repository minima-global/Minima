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

	String mUID;

	String mHost;

	int mPort;

	boolean isIncoming;

	public NIOClientInfo(String uid, String zHost, int zPort, boolean zIsIncoming){
		mConnected = true;
		mUID = uid;
		mHost = zHost;
		mPort = zPort;
		isIncoming = zIsIncoming;
	}

	public NIOClientInfo(NIOClient zNIOClient, boolean zConnected) {
		mNIOClient 	= zNIOClient;
		mConnected	= zConnected;
		mHost = zNIOClient.getHost();
		mPort = zNIOClient.getPort();
		mUID = zNIOClient.getUID();
	}
	
	public String getUID() {
		return mUID;
	}
	
	public boolean isIncoming() {
		return isIncoming;
	}
	
	public boolean isConnected() {
		return mConnected;
	}
	
	public String getHost() {return mHost;}
	
	public int getPort() {
		return mPort;
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
		if (mNIOClient != null) {
			ret.put("connected", new Date(getTimeConnected()).toString());
		}
		
		return ret;
	}
}
