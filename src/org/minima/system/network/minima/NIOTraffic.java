package org.minima.system.network.minima;

import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONObject;

public class NIOTraffic {

	long mStartTime;
	long mTotalRead;
	long mTotalWrite;
	
	Hashtable<String, Long> mReadBreakDown 	= new Hashtable<>();
	Hashtable<String, Long> mWriteBreakDown = new Hashtable<>();
	
	public NIOTraffic() {
		reset();
	}
	
	public synchronized void reset() {
		mStartTime 	= System.currentTimeMillis();
		mTotalRead 	= 0;
		mTotalWrite = 0;
		
		mReadBreakDown = new Hashtable<>();
		mWriteBreakDown = new Hashtable<>();
	}
	
	public synchronized long getStartTime() {
		return mStartTime;
	}
	
	public synchronized long getTotalRead() {
		return mTotalRead;
	}
	
	public synchronized long getTotalWrite() {
		return mTotalWrite;
	}
	
	public synchronized JSONObject getBreakdown() {
		JSONObject ret = new JSONObject();
		
		//First the Reads
		JSONObject rr = new JSONObject();
		Enumeration<String> reads = mReadBreakDown.keys();
		while(reads.hasMoreElements()) {
			String key = reads.nextElement();
			Long value = mReadBreakDown.get(key);
			String vs = MiniFormat.formatSize(value.longValue());
			rr.put(key, vs);
		}
		
		//Writes
		JSONObject ww = new JSONObject();
		Enumeration<String> writes = mWriteBreakDown.keys();
		while(writes.hasMoreElements()) {
			String key = writes.nextElement();
			Long value = mWriteBreakDown.get(key);
			String vs = MiniFormat.formatSize(value.longValue());
			ww.put(key, vs);
		}
		
		//Add to main JSON
		ret.put("reads", rr);
		ret.put("writes", ww);
		
		return ret;
	}
	
	public synchronized void addToTotalRead(long zAmount) {
		mTotalRead += zAmount;
	}
	
	public synchronized void addToTotalWrite(long zAmount) {
		mTotalWrite += zAmount;
	}
	
	public synchronized void addReadBytes(String zFrom, long zAmount) {
		//mTotalRead += zAmount;
		
		long current=0;
		if(mReadBreakDown.containsKey(zFrom)) {
			current = mReadBreakDown.get(zFrom).longValue();
		}
		
		mReadBreakDown.put(zFrom, Long.valueOf(current+zAmount));
	}
	
	public synchronized void addWriteBytes(String zFrom, long zAmount) {
		//mTotalWrite += zAmount;
		
		long current=0;
		if(mWriteBreakDown.containsKey(zFrom)) {
			current = mWriteBreakDown.get(zFrom).longValue();
		}
		
		mWriteBreakDown.put(zFrom, Long.valueOf(current+zAmount));
	}
}
