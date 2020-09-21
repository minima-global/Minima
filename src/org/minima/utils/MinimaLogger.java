/**
 * 
 */
package org.minima.utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import org.minima.system.brains.ConsensusHandler;
import org.minima.utils.messages.Message;

/**
 * @author Spartacus Rex
 *
 */
public class MinimaLogger {
	
	public static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss", Locale.ENGLISH );
	
	public static boolean LOGGING_ON 	 = true;
	
	public static final int LOG_ERROR 	 = 0;
	public static final int LOG_INFO 	 = 1;
	
	/**
	 * Previous Output..
	 */
	public static int MAX_PREV_LEN = 10000;
	private static StringBuffer mFullOutput = new StringBuffer();
	public static String getFullOutput() {
		return mFullOutput.toString();
	}

	/**
	 * Send LOG messages to all those listening...
	 */
	static ConsensusHandler mLogHandler = null;
	
	public static void setMainHandler(ConsensusHandler zLogHandler) {
		mLogHandler = zLogHandler;
	}
	
	public static void log(String zLog){
		if(LOGGING_ON){
			String full_log = "Minima @ "+DATEFORMAT.format(new Date())+" : "+zLog;
			System.out.println(full_log);
	
			//Store..
			mFullOutput.append(full_log+"\n");
			
			//Ensure max size..
			int len = mFullOutput.length();
			if(len>MAX_PREV_LEN) {
				mFullOutput = new StringBuffer(mFullOutput.substring(len-MAX_PREV_LEN, len));
			}
			
			//Forward to listeners..
			if(mLogHandler != null) {
				Message log = new Message(ConsensusHandler.CONSENSUS_NOTIFY_LOG);
				log.addString("msg", full_log);
				mLogHandler.updateListeners(log);
			}
		}
	}	
}
