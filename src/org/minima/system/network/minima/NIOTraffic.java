package org.minima.system.network.minima;

import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.utils.json.JSONObject;

public class NIOTraffic {

	public long mStartTime;
	public long mTotalRead;
	public long mTotalWrite;
	
	Hashtable<String, Long> mReadBreakDown 	= new Hashtable<>();
	Hashtable<String, Long> mWriteBreakDown = new Hashtable<>();
	
	public NIOTraffic() {
		reset();
	}
	
	public void reset() {
		mStartTime 	= System.currentTimeMillis();
		mTotalRead 	= 0;
		mTotalWrite = 0;
		
		mReadBreakDown = new Hashtable<>();
		mWriteBreakDown = new Hashtable<>();
	}
	
	public long getStartTime() {
		return mStartTime;
	}
	
	public long getTotalRead() {
		return mTotalRead;
	}
	
	public long getTotalWrite() {
		return mTotalWrite;
	}
	
	public JSONObject getBreakdown() {
		JSONObject ret = new JSONObject();
		
		//First the Reads
		JSONObject rr = new JSONObject();
		Enumeration<String> reads = mReadBreakDown.keys();
		while(reads.hasMoreElements()) {
			String key = reads.nextElement();
			Long value = mReadBreakDown.get(key);
			rr.put(key, value);
		}
		
		//Writes
		JSONObject ww = new JSONObject();
		Enumeration<String> writes = mWriteBreakDown.keys();
		while(writes.hasMoreElements()) {
			String key = writes.nextElement();
			Long value = mWriteBreakDown.get(key);
			ww.put(key, value);
		}
		
		//Add to main JSON
		ret.put("reads", rr);
		ret.put("writes", ww);
		
		return ret;
	}
	
//	public void addReadBytes(String zFrom, int zRead) {
//		addReadBreakAmount(zFrom, zRead);
//	}
	
//	public void addWriteBytes(int zWrite) {
//		mTotalWrite += zWrite;
//	}
	
	public void addReadBytes(String zFrom, long zAmount) {
		mTotalRead += zAmount;
		
		long current=0;
		if(mReadBreakDown.containsKey(zFrom)) {
			current = mReadBreakDown.get(zFrom).longValue();
		}
		
		mReadBreakDown.put(zFrom, Long.valueOf(current+zAmount));
	}
	
	public void addWriteBytes(String zFrom, long zAmount) {
		mTotalWrite += zAmount;
		
		long current=0;
		if(mWriteBreakDown.containsKey(zFrom)) {
			current = mWriteBreakDown.get(zFrom).longValue();
		}
		
		mWriteBreakDown.put(zFrom, Long.valueOf(current+zAmount));
	}
}
