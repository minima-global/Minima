package org.minima.system.network.maxima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MaximaMessage implements Streamable {

	/**
	 * The Time Milli
	 */
	MiniNumber mTimeMilli = new MiniNumber(System.currentTimeMillis());
	
	/**
	 * Who is this message from
	 */
	public MiniString mFromAddress;
	
	/**
	 * Who To - The Public Key
	 */
	public MiniData mToPublic;
	
	/**
	 * The PORT - or application..
	 */
	public MiniString mApplication;
	
	/**
	 * The Data
	 */
	public MiniData mData;
	
	/**
	 * The Signature
	 */
	public MiniData mSignature;
	
	public MaximaMessage() {}

	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("time", new Date(mTimeMilli.getAsLong()));
		ret.put("from", mFromAddress.toString());
		ret.put("to", mToPublic.to0xString());
		ret.put("application", mApplication.toString());
		ret.put("data", mData.to0xString());
		ret.put("signature", mSignature.to0xString());
		
		return ret;
	}
	
	public static MaximaMessage ConvertMiniDataVersion(MiniData zData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MaximaMessage mm = new MaximaMessage();
		
		try {
			mm.readDataStream(dis);
			
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return mm;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTimeMilli.writeDataStream(zOut);
		mFromAddress.writeDataStream(zOut);
		mToPublic.writeDataStream(zOut);
		mApplication.writeDataStream(zOut);
		mData.writeDataStream(zOut);
		mSignature.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTimeMilli		= MiniNumber.ReadFromStream(zIn);
		mFromAddress 	= MiniString.ReadFromStream(zIn);
		mToPublic		= MiniData.ReadFromStream(zIn);
		mApplication	= MiniString.ReadFromStream(zIn);
		mData 			= MiniData.ReadFromStream(zIn);
		mSignature 		= MiniData.ReadFromStream(zIn);
	}
	
	public static MaximaMessage ReadFromStream(DataInputStream zIn) throws IOException {
		MaximaMessage mm = new MaximaMessage();
		mm.readDataStream(zIn);
		return mm;
	}
}
