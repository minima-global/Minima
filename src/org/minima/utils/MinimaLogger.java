/**
 * 
 */
package org.minima.utils;

/**
 * @author Spartacus Rex
 *
 */
public class MinimaLogger {

//	public static final String NATIVE_SYSTEM_OUT = "NATIVE_SYSTEM_OUT";
	
	public static boolean LOGGING_ON 	 = true;
	
	public static final int LOG_ERROR 	 = 0;
	public static final int LOG_INFO 	 = 1;
	
//	private static NetClient mRemoteHost = null;
//	public static void setRemoteClient(NetClient zRemoteHost) {
//		if(mRemoteHost == null && zRemoteHost!=null) {
//			System.out.println("REMOTE SET : "+zRemoteHost);	
//		}else if (mRemoteHost != null && zRemoteHost==null){
//			System.out.println("REMOTE RESET");
//		}
//		
//		mRemoteHost = zRemoteHost;
//	}
	
//	public static void log(){
//		log("");
//	}
	
//	private static String mFullOutput = "";
//	public static String getFullOutput() {
//		return mFullOutput;
//	}
//	
//	private static NativeListener mOutputListener = null;
//	public static void setNativeListener(NativeListener zListener) {
//		mOutputListener = zListener;
//	}
	
	public static void log(String zLog){
		if(LOGGING_ON){
			System.out.println(zLog);
	
//			//Send to Native..
//			if(mOutputListener != null) {
//				mOutputListener.processMessage(new Message(NATIVE_SYSTEM_OUT).addString("log", zLog));
//			}
//			
//			//Add to the rolling Total Output
//			mFullOutput = mFullOutput.concat(zLog+"\n");
//			int len = mFullOutput.length();
//			if(len > 10000) {
//				mFullOutput = mFullOutput.substring(len-10000,len);	
//			}
//			
//			//Remote Connection..
//			if(mRemoteHost != null) {
//				Message msg = new Message(NetClient.NETCLIENT_SENDOBJECT);
//				msg.addObject("type", NetClientReader.NETMESSAGE_REMOTE);
//				msg.addObject("object", new MiniData(zLog.getBytes()));
//				mRemoteHost.PostMessage(msg);
//			}
		}
	}	
}
