package org.minima.system.network.maxima.db;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.utils.Streamable;

public class MaximaUser implements Streamable {

	public String mPublicKey = new String();
	
	public String mHost = new String();
	
	public long mTimeStamp = 0;
	
	public MaximaUser(String zPubkey, String zHost, long zTimeStamp) {
		setPublicKey(zPubkey);
		setHost(zHost);
		mTimeStamp = zTimeStamp;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public void setPublicKey(String zPubKey) {
		mPublicKey = zPubKey;
	}
	
	public String getHost() {
		return mHost;
	}
	
	public void setHost(String zHost) {
		mHost = zHost;
	}
	
	public long getTimeStamp() {
		return mTimeStamp;
	}
	
	public void setTimeStamp(long zTimeStamp) {
		mTimeStamp = zTimeStamp;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		// TODO Auto-generated method stub
		
	}
}
