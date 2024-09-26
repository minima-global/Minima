package org.minima.utils;

import java.lang.Thread.UncaughtExceptionHandler;

public class MinimaUncaughtException implements UncaughtExceptionHandler {
	
	//The default Uncaught Exception.. 
	private static UncaughtExceptionHandler mDefaultUnCaught;

	public MinimaUncaughtException() {
		//Get the current default
		mDefaultUnCaught = Thread.getDefaultUncaughtExceptionHandler();
	}
	
	@Override
	public void uncaughtException(Thread zThread, Throwable zThrowable) {
		MinimaLogger.log("[!] UNCAUGHT EXCEPTION at THREAD "+zThread.getName());
		MinimaLogger.logUncaught(zThrowable,true);
		
		//What type of error is it!
		if(zThrowable instanceof java.lang.OutOfMemoryError) {
			MinimaLogger.log("[!] MEMORY ERROR.. SHUTTING DOWN");
			
			Runtime.getRuntime().halt(0);
			//System.exit(0);
			
			return;
			
		}else if(zThrowable instanceof org.h2.mvstore.MVStoreException) {
			MinimaLogger.log("[!] H2 DATABASE MVStoreException.. SHUTTING DOWN");
			
			Runtime.getRuntime().halt(0);
			//System.exit(0);
			
			return;
		
		}else if(zThrowable instanceof java.util.concurrent.TimeoutException) {
			MinimaLogger.log("[!] Concurrent Timeout Exception.. SHUTTING DOWN");
			
			Runtime.getRuntime().halt(0);
			//System.exit(0);
			
			return;
		}
		
		//Otherwise pass to the default..
		MinimaLogger.log("[!] PASSING UNCAUGHT ERROR TO DEFAULT HANDLER");
		mDefaultUnCaught.uncaughtException(zThread, zThrowable);
	}
}
