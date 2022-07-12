package org.minima.system.network.maxima.mls;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MLSPacketGETResp implements Streamable {

	String mRandomUID;
	String mPublicKey;
	String mCurrentAddress;

	private MLSPacketGETResp() {}
	
	public MLSPacketGETResp(String zPublicKey, String zMaximaAddress, String zRandomUID){
		mPublicKey		= zPublicKey;
		mCurrentAddress = zMaximaAddress;
		mRandomUID		= zRandomUID;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public String getAddress() {
		return mCurrentAddress;
	}

	public String getRandomUID() {
		return mRandomUID;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("publickey", getPublicKey());
		json.put("address", getAddress());
		json.put("ranmdomuid", getRandomUID());
		
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniString.WriteToStream(zOut, mPublicKey);
		MiniString.WriteToStream(zOut, mCurrentAddress);
		MiniString.WriteToStream(zOut, mRandomUID);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey 		= MiniString.ReadFromStream(zIn).toString();
		mCurrentAddress = MiniString.ReadFromStream(zIn).toString();
		mRandomUID		= MiniString.ReadFromStream(zIn).toString();
	}
	
	public static MLSPacketGETResp ReadFromStream(DataInputStream zIn) throws IOException {
		MLSPacketGETResp mls = new MLSPacketGETResp();
		mls.readDataStream(zIn);
		return mls;
	}
	
	/**
	 * Convert a MiniData version into a MLSPacketGET
	 */
	public static MLSPacketGETResp convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MLSPacketGETResp mls = null;
		
		try {
			mls = MLSPacketGETResp.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			//MinimaLogger.log(e);
		}
		
		return mls;
	}
}
