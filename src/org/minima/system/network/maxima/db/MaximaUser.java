package org.minima.system.network.maxima.db;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MaximaUser implements Streamable {

	public MiniString mPublicKey;
	
	public MiniString mHost;
	
	public MiniNumber mTimeStamp = MiniNumber.ZERO;
	
	public MaximaUser() {}

	public MaximaUser(String zPubkey, String zHost) {
		setPublicKey(zPubkey);
		setHost(zHost);
		setTimeStamp(System.currentTimeMillis());
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("publickey", mPublicKey.toString());
		json.put("host",mHost.toString());
		json.put("timestamp", new Date(mTimeStamp.getAsLong()));
		
		return json;
	}
	
	public String getPublicKey() {
		return mPublicKey.toString();
	}
	
	public void setPublicKey(String zPubKey) {
		mPublicKey = new MiniString(zPubKey);
	}
	
	public String getHost() {
		return mHost.toString();
	}
	
	public void setHost(String zHost) {
		mHost = new MiniString(zHost);
		setTimeStamp(System.currentTimeMillis());
	}
	
	public String getCompleteAddress() {
		return getPublicKey()+"@"+getHost();
	}
	
	public long getTimeStamp() {
		return mTimeStamp.getAsLong();
	}
	
	public void setTimeStamp(long zTimeStamp) {
		mTimeStamp = new MiniNumber(zTimeStamp);
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mHost.writeDataStream(zOut);
		mTimeStamp.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey = MiniString.ReadFromStream(zIn);
		mHost = MiniString.ReadFromStream(zIn);
		mTimeStamp = MiniNumber.ReadFromStream(zIn);
	}
	
	
}
