package org.minima.system.network.maxima.db;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;

public class MaximaUser implements Streamable {

	public MiniString mPublicKey;
	
	public MiniString mHost;
	
	public MiniNumber mTimeStamp = MiniNumber.ZERO;
	
	public MaximaUser(String zPubkey, String zHost, long zTimeStamp) {
		setPublicKey(zPubkey);
		setHost(zHost);
		setTimeStamp(zTimeStamp);
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
