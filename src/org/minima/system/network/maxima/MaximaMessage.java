package org.minima.system.network.maxima;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;

public class MaximaMessage implements Streamable {

	/**
	 * Who is this message from
	 */
	public MiniString mFromAddress;
	
	/**
	 * Who is it to
	 */
	public MiniString mTo;
	
	/**
	 * The Data
	 */
	public MiniData mData;
	
	/**
	 * The Signature
	 */
	public MiniData mSignature;
	
	public MaximaMessage() {}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mFromAddress.writeDataStream(zOut);
		mTo.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mFromAddress = MiniString.ReadFromStream(zIn);
		mTo = MiniString.ReadFromStream(zIn);
		mData = MiniData.ReadFromStream(zIn);
	}
}
