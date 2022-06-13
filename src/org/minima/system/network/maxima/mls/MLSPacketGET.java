package org.minima.system.network.maxima.mls;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.utils.Streamable;

public class MLSPacketGET implements Streamable {

	MiniString mCurrentAddress;
	
	public MLSPacketGET(String zMaximaAddress){
		
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//First write out a special data..
		MaximaManager.MAXIMA_MLS_START.writeDataStream(zOut);
		
		//And NOW write out the data
		mCurrentAddress.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//First the special beginning..
		MiniData prefix = MiniData.ReadFromStream(zIn);
		
		//And now the current address..
		mCurrentAddress = MiniString.ReadFromStream(zIn);
	}
}
