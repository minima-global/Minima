package org.minima.system.network.minima;

public class NIOTraffic {

	public long mStartTime;
	public long mTotalRead;
	public long mTotalWrite;
	
	public NIOTraffic() {
		reset();
	}
	
	public void reset() {
		mStartTime 	= System.currentTimeMillis();
		mTotalRead 	= 0;
		mTotalWrite = 0;
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
	
	public void addReadBytes(int zRead) {
		mTotalRead += zRead;
	}
	
	public void addWriteBytes(int zWrite) {
		mTotalWrite += zWrite;
	}
}
