package org.minima.system.mds;

import java.io.File;

import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniString;
import org.minima.system.mds.runnable.MDSJS;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.mozilla.javascript.ClassShutter;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class ServiceJSRunner extends MessageProcessor{

	public static String SERVICEJS_INIT = "SERVICEJS_INIT";
	public static String SERVICEJS_STOP = "SERVICEJS_STOP";
	public static String SERVICEJS_POLL = "SERVICEJS_POLL";
	
	MDSManager mMDS;
	
	MiniDAPP mMiniDapp;
	
	MDSJS mMDSJS;
	
	public ServiceJSRunner(MiniDAPP zDapp, MDSManager zMDS) {
		super("SERVICEJS_RUUNNER");
		
		mMiniDapp = zDapp;
		mMDS 	  = zMDS;
		
		PostMessage(SERVICEJS_INIT);
	}
	
	public MiniDAPP getMiniDapp() {
		return mMiniDapp;
	}
	
	public String getMiniDappID() {
		return mMiniDapp.getUID();
	}
	
	public void stopJS() {
		
		//Clear the message stack
		clear();
		
		//Send the shutdown message - must be from the processor thread
		PostMessage(SERVICEJS_STOP);
	}
	
	public void sendPollMessage(JSONObject zPollObject) {
		Message pollmessage = new Message(SERVICEJS_POLL);
		pollmessage.addObject("poll_object", zPollObject);
		PostMessage(pollmessage);
	}
	
	/**
	 * Initialise a MiniDAPP
	 */
	private void setupMiniDAPP() {
		
		//Is there a service.js class
		File service = new File(mMDS.getMiniDAPPWebFolder(mMiniDapp.getUID()),"service.js");
		if(service.exists()) {
			
			try {
				//Load the file..
				byte[] serv = MiniFile.readCompleteFile(service);
				String code = new String(serv,MiniString.MINIMA_CHARSET);
				
				//Load it into the service runner..
				Context ctx = Context.enter();
				
				ctx.setOptimizationLevel(-1);
				ctx.setLanguageVersion(Context.VERSION_ES6);
				ctx.setMaximumInterpreterStackDepth(1024);
				
				//Stop JAVA classes from being run..
				//try {
					ctx.setClassShutter(new ClassShutter() {
						public boolean visibleToScripts(String className) {					
							
							//ONLY MDSJS can be called form JS
							if(className.startsWith("org.minima.system.mds.runnable")) {
								return true;
							}
								
							//MinimaLogger.log("RHINOJS JAVA CLASS DENIED ACCESS : "+className);
							
							return false;
						}
					});
				/*}catch(SecurityException sec) {
					//MinimaLogger.log(sec.toString());
				}*/
				
				//Create the Scope
				Scriptable scope = ctx.initStandardObjects();
				
				//Create an MDSJS object
				mMDSJS = new MDSJS(mMDS, mMiniDapp.getUID(), mMiniDapp.getName(), ctx, scope);
				ScriptableObject.putProperty(scope, "MDS", Context.javaToJS(mMDSJS, scope));
				
				//Add the main code to the Runnable
				ctx.evaluateString(scope, code, "<mds_"+mMiniDapp.getName()+"_"+mMiniDapp.getUID()+">", 1, null);
			
			}catch(Exception exc) {
				MinimaLogger.log("ERROR starting service "+mMiniDapp.getName()+" "+exc);
			}
		}else {
			MinimaLogger.log("ERROR starting service with no service.js : "+mMiniDapp.getName());
		}
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(SERVICEJS_INIT)) {
			
			//Start the service
			setupMiniDAPP();
		
			MinimaLogger.log("Started service.js "+mMiniDapp.getName());
			
		}else if(zMessage.getMessageType().equals(SERVICEJS_STOP)) {
		
			//MinimaLogger.log("ServiceJS Stopped : "+mMiniDapp.getName());
			
			try {
				mMDSJS.shutdown();
			}catch (Exception e) {
				MinimaLogger.log(e);
			}
			
			stopMessageProcessor();
			
		}else if(zMessage.getMessageType().equals(SERVICEJS_POLL)) {
			
			JSONObject pollobj = (JSONObject) zMessage.getObject("poll_object");
			
			if(pollobj == null) {
				MinimaLogger.log("NULL POLL");
				return;
			}
			
			mMDSJS.callMainCallback(pollobj);
		}
	}
	
}

