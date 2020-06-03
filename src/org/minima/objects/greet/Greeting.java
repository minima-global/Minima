package org.minima.objects.greet;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;

public class Greeting implements Streamable {
		
	/**
	 * What version are we..
	 */
	MiniString mVersion = new MiniString(GlobalParams.MINIMA_VERSION);
	
	/**
	 * A complete list of all the hashes in our blockchain 
	 */
	ArrayList<HashNumber> mTxPowList = new ArrayList<>(); 
	
	public Greeting() {}
	
	public void addBlock(MiniData zHash, MiniNumber zBlockNumber){
		HashNumber hn = new HashNumber(zHash,zBlockNumber);
		mTxPowList.add(hn);
	}
	
	public ArrayList<HashNumber> getList() {
		return mTxPowList;
	}
	
	public String getVersion() {
		return mVersion.toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//First the version.. 
		mVersion.writeDataStream(zOut);
		
		//Next the details..
		int len = mTxPowList.size();
		MiniNumber size = new MiniNumber(len);
		size.writeDataStream(zOut);
		
		//Write it all out..
		for(HashNumber hntxpow : mTxPowList) {
			hntxpow.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVersion = MiniString.ReadFromStream(zIn);
		MiniNumber minlen = MiniNumber.ReadFromStream(zIn);
	
		mTxPowList = new ArrayList<>();
		int len = minlen.getAsInt();
		for(int i=0;i<len;i++) {
			HashNumber hn = HashNumber.ReadFromStream(zIn);
			mTxPowList.add(hn);
		}
	}
	
	public static Greeting ReadFromStream(DataInputStream zIn) throws IOException {
		Greeting greet = new Greeting();
		greet.readDataStream(zIn);
		return greet;
	}
}
