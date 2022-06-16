package org.minima.system.network.maxima.mls;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MLSPacketSET implements Streamable {

	long mTimeMilli;
	
	MiniString mMaximaIdentity;
	
	ArrayList<String> mValidPubKeys;
	
	private MLSPacketSET() {}
	
	public MLSPacketSET(String zIdentity) {
		mMaximaIdentity = new MiniString(zIdentity);
		mValidPubKeys 	= new ArrayList<>();
	}
	
	public long getMilliTime(){
		return mTimeMilli;
	}
	
	public String getMaximaAddress() {
		return mMaximaIdentity.toString();
	}
	
	public boolean isValidPublicKey(String zPublicKey) {
		return mValidPubKeys.contains(zPublicKey);
	}
	
	public void addValidPublicKey(String zPublicKey) {
		mValidPubKeys.add(zPublicKey);
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("maximaidentity", mMaximaIdentity);
		
		JSONArray valid = new JSONArray();
		for(String vv : mValidPubKeys) {
			valid.add(vv);
		}
		ret.put("valid", valid);
		
		return ret;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mMaximaIdentity.writeDataStream(zOut);
		MiniNumber.WriteToStream(zOut, mValidPubKeys.size());
		for(String pubkey : mValidPubKeys) {
			MiniString.WriteToStream(zOut, pubkey);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mMaximaIdentity = MiniString.ReadFromStream(zIn);
		mValidPubKeys = new ArrayList<>();
		int size = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<size;i++) {
			mValidPubKeys.add(MiniString.ReadFromStream(zIn).toString());
		}
		
		//This is done manually - so can't be spoofed
		mTimeMilli = System.currentTimeMillis(); 
	}

	public static MLSPacketSET ReadFromStream(DataInputStream zIn) throws IOException {
		MLSPacketSET mls = new MLSPacketSET();
		mls.readDataStream(zIn);
		return mls;
	}
	
	/**
	 * Convert a MiniData version into a MLSPacketSET
	 */
	public static MLSPacketSET convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MLSPacketSET mls = null;
		
		try {
			mls = MLSPacketSET.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return mls;
	}
}
