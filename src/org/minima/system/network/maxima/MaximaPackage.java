package org.minima.system.network.maxima;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;

public class MaximaPackage implements Streamable {

	//What version are we..
	MiniString mVersion = new MiniString("1.0");
	
	//Who this message is for.. The Public Key ONLY
	MiniData mTo;
	
	//The ENCRYPTED data - A MaximaInternal data structure
	MiniData mData;
	
	private MaximaPackage() {};
	
	public MaximaPackage(MiniData zTo, MiniData zData) {
		mTo 	= zTo;
		mData 	= zData;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mVersion.writeDataStream(zOut);
		mTo.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVersion 	= MiniString.ReadFromStream(zIn);
		mTo			= MiniData.ReadFromStream(zIn);
		mData		= MiniData.ReadFromStream(zIn);
	}
	
	public static MaximaPackage ReadFromStream(DataInputStream zIn) throws IOException {
		MaximaPackage mp = new MaximaPackage();
		mp.readDataStream(zIn);
		return mp;
	}
}
