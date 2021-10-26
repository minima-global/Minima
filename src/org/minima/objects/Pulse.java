package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;

public class Pulse implements Streamable {

	public static int PULSE_VERSION = 1; 
	
	/**
	 * A list of the latest block hashes ( not all - just the last 20 minutes )
	 */
	ArrayList<MiniData> mBlockList;
	
	/**
	 * A recent piece of Work
	 */
	TxPoW mPulsePoW;
	
	public Pulse() {
		mBlockList = new ArrayList<>();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		// TODO Auto-generated method stub
		
	}

}
