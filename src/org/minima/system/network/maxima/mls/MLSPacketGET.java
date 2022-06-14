package org.minima.system.network.maxima.mls;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MLSPacketGET implements Streamable {

	String mPublicKey;
	String mCurrentAddress;

	private MLSPacketGET() {}
	
	public MLSPacketGET(String zPublicKey, String zMaximaAddress){
		mPublicKey		= zPublicKey;
		mCurrentAddress = zMaximaAddress;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public String getAddress() {
		return mCurrentAddress;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniString.WriteToStream(zOut, mPublicKey);
		MiniString.WriteToStream(zOut, mCurrentAddress);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey 		= MiniString.ReadFromStream(zIn).toString();
		mCurrentAddress = MiniString.ReadFromStream(zIn).toString();
	}
	
	public static MLSPacketGET ReadFromStream(DataInputStream zIn) throws IOException {
		MLSPacketGET mls = new MLSPacketGET();
		mls.readDataStream(zIn);
		return mls;
	}
	
	/**
	 * Convert a MiniData version into a MLSPacketGET
	 */
	public static MLSPacketGET convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MLSPacketGET mls = null;
		
		try {
			mls = MLSPacketGET.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return mls;
	}
}
