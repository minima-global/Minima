package org.minima.utils;

import java.util.ArrayList;

import org.minima.objects.base.MiniData;

public class TimedRequest {

	public class RequestUnit {
		
		public long mTimeMilli;
		public MiniData mDataID;
		
		public RequestUnit(MiniData zData) {
			mTimeMilli 	= System.currentTimeMillis();
			mDataID 	= zData;
		}
	}
	
	
	public ArrayList<RequestUnit> mPreviousRequests;
	
	public TimedRequest() {
		mPreviousRequests = new ArrayList<>();
	}
	
	public boolean sendNow(MiniData zDataID) {
		
		//Have we recently sent this request..
		for(RequestUnit requnit : mPreviousRequests) {
			if(requnit.mDataID.isEqual(zDataID)) {
				return false;
			}
		}
		
		return true;
	}
}
