package org.minima.system.network.minidapps.minilib;

import org.minima.utils.ProtocolException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Callable;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class BackBoneDAPP {

	/**
	 * JavaScript
	 */
	String mScript;
	
	/**
	 * The Context it is running in
	 */
	Context mContext;
	
	/**
	 * The Script Scope
	 */
	Scriptable mScope;
	
	/**
	 * The MinimaEvent Function inside the scope
	 */
	Function mMinimEventJS;
	
	public BackBoneDAPP(String zScriptJS) throws ProtocolException {
		//The JavaScrit
		mScript = zScriptJS;
		
		//Context and scope of the JS backend
		mContext = Context.enter();
		mScope   = mContext.initStandardObjects();
	
		//Create a MinimaJS object for this scope..
		MinimaJS minijs = new MinimaJS("0x01",this);
		
		//Add it to this environment
		Object wrappedMinima = Context.javaToJS(minijs, mScope);
		ScriptableObject.putProperty(mScope, "Minima", wrappedMinima);
	
		//Evaluate the script
		mContext.evaluateString(mScope, zScriptJS, "<cmd>", 1, null);
	
		//Get the function
		Object fObj = mScope.get("MinimaEvent", mScope);
		
		if (!(fObj instanceof Function)) {
		    throw new ProtocolException("MinimaEvent is undefined or not a function.");
		} 
		
		//Store for later
		mMinimEventJS = (Function)fObj;
	}
	
	public Context getContext() {
		return mContext;
	}
	
	public Scriptable getScope() {
		return mScope;
	}
	
	public void shutdown() {
		mContext.exit();
	}
	
	/**
	 * When an Event occurs send the message to the JS scope
	 * 
	 * - newblock
	 * - newtransction
	 * - newbalance
	 */
	public void MinimaEvent(String zJSONEvent) {
		//Create a JS JSONObject
		Object json = makeJSONObject(zJSONEvent, mContext, mScope);
		
		//Make a function variable list
		Object functionArgs[] = { json };
	    
		//Call the Function..
		mMinimEventJS.call(mContext, mScope, mScope, functionArgs);
	}
	
	/**
	 * Weird little function to convert a Java JSONObject to a JS one..
	 * @param zJSON
	 * @param rhino
	 * @param scope
	 * 
	 * @return The JS JSONObect
	 */
	public static Object makeJSONObject(String zJSON, Context rhino, Scriptable scope) {
	    Object param = NativeJSON.parse(rhino, scope, zJSON, new Callable() {
			@Override
			public Object call(Context arg0, Scriptable arg1, Scriptable arg2, Object[] arg3) {
				return arg3[1];
			}
		});
	    return param;
	}
	
	//tester
	public static void main(String[] zArgs) {
	
		String js = "\n" + 
				"var tot = 0;\n" + 
				"\n" + 
				"/** \n" + 
				"* Main Entry Point..\n" + 
				"*/\n" + 
				"function MinimaEvent(evt){\n" + 
				"\n" + 
				"	var jsonstr = JSON.stringify(evt,null,2);\n" + 
				"	\n" + 
				"	Minima.log(\"MinimaEvent : \"+tot+\") \"+jsonstr);\n" + 
				"	tot++;\n" + 
				"	\n" + 
				"	Minima.cmd( \"help;balance\", function(respjson){\n" + 
				"		var respstr = JSON.stringify(respjson,null,2);\n" + 
				"	\n" + 
				"		//Convert the object to a json string..\n" + 
				"		Minima.log(\"CMD RESPONSE : \"+respstr);\n" + 
				"	});\n" + 
				"}\n" + 
				"";
		
		try {
			BackBoneDAPP bdapp = new BackBoneDAPP(js);

			//And send a JSON msg..
			JSONObject newblock = new JSONObject();
			newblock.put("event", "newblock");
			newblock.put("status", "the status");
			
			JSONArray testarray = new JSONArray();
			testarray.add(newblock);
			
			bdapp.MinimaEvent(testarray.toString());
		
			bdapp.shutdown();
		
		} catch (ProtocolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}
	
	
}
