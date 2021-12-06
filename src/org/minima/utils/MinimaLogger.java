/**
 * 
 */
package org.minima.utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import org.minima.system.Main;
import org.minima.utils.messages.Message;

/**
 * @author Spartacus Rex
 *
 */
public class MinimaLogger {
	
	public static final String MINIMA_LOG = "MINIMA_LOG";
	
	public static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss", Locale.ENGLISH );
	
	public static void log(String zLog){
		long mem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		String full_log = "Minima @ "+DATEFORMAT.format(new Date())+" ["+MiniFormat.formatSize(mem)+"] : "+zLog;
		System.out.println(full_log);
		
		//Is there a listener..
		Main.postMinimaListener(new Message(MINIMA_LOG).addString("log", full_log));	
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
