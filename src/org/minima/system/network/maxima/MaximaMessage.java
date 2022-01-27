package org.minima.system.network.maxima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MaximaMessage implements Streamable {

	/**
	 * A Random Data value so EVERY message hash is Unique
	 */
	MiniData mRandom = MiniData.getRandomData(32);
	
	/**
	 * The Time Milli
	 */
	public MiniNumber mTimeMilli = new MiniNumber(System.currentTimeMillis());
	
	/**
	 * Who it is From
	 */
	public MiniString mFrom;
	
	/**
	 * Who it is to
	 */
	public MiniString mTo;
	
	/**
	 * The Application / Port that this message is aimed at
	 */
	public MiniString mApplication;
	
	/**
	 * The Actual Data
	 */
	public MiniData mData;
	
	public MaximaMessage() {}

	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("from", mFrom.toString());
		ret.put("to", mTo.toString());
		ret.put("time", new Date(mTimeMilli.getAsLong()).toString());
		ret.put("timemilli", mTimeMilli);
		ret.put("random", mRandom.to0xString());
		ret.put("application", mApplication.toString());
		ret.put("data", mData.to0xString());
		
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
		mRandom.writeDataStream(zOut);
		mFrom.writeDataStream(zOut);
		mTo.writeDataStream(zOut);
		mTimeMilli.writeDataStream(zOut);
		mApplication.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mRandom			= MiniData.ReadFromStream(zIn);
		mFrom			= MiniString.ReadFromStream(zIn);
		mTo				= MiniString.ReadFromStream(zIn);
		mTimeMilli		= MiniNumber.ReadFromStream(zIn);
		mApplication	= MiniString.ReadFromStream(zIn);
		mData 			= MiniData.ReadFromStream(zIn);
	}
	
	public static MaximaMessage ReadFromStream(DataInputStream zIn) throws IOException {
		MaximaMessage mm = new MaximaMessage();
		mm.readDataStream(zIn);
		return mm;
	}
}

