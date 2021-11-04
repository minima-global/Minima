/**
 * 
 */
package org.minima.utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;

/**
 * @author Spartacus Rex
 *
 */
public class MinimaLogger {
	
	static MessageListener mLogListener = null;
	
	public static void setListener(MessageListener zLogListener) {
		mLogListener = zLogListener;
	}
	
	public static void resetListener() {
		mLogListener = null;
	}
	
	public static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss", Locale.ENGLISH );
	
	public static void log(String zLog){
		long mem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		String full_log = "Minima @ "+DATEFORMAT.format(new Date())+" ["+MiniFormat.formatSize(mem)+"] : "+zLog;
		System.out.println(full_log);
		
		//Is there a listener..
		if(mLogListener != null) {
			mLogListener.processMessage(new Message("").addString("log", full_log));
		}
	}
	
	public static void log(Exception zException){
		//First the Full Exception
		MinimaLogger.log(zException.toString());
		
		//Now the Stack Trace
		for(StackTraceElement stack : zException.getStackTrace()) {
			//Print it..
			MinimaLogger.log("     "+stack.toString());
		}
	}
}
