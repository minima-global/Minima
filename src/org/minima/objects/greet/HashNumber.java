package org.minima.objects.greet;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class HashNumber implements Streamable {
	MiniData   mHash;
	MiniNumber mNumber;
	
	public HashNumber() {}
	
	public HashNumber(MiniData zHash, MiniNumber zNumber) {
		mHash = zHash;
		mNumber = zNumber;
	}
	
	public MiniData getHash() {
		return mHash;
	}
	
	public MiniNumber getNumber() {
		return mNumber;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mHash.writeHashToStream(zOut);
		mNumber.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mHash = MiniData.ReadHashFromStream(zIn);
		mNumber = MiniNumber.ReadFromStream(zIn);
	}
	
	public static HashNumber ReadFromStream(DataInputStream zIn) throws IOException {
		HashNumber hn = new HashNumber();
		hn.readDataStream(zIn);
		return hn;
	}
}