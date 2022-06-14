package org.minima.system.mds.runnable;

import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Callable;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ast.Scope;

public class MDSJS {

	public class NullCallable implements Callable
	{
	    @Override
	    public Object call(Context context, Scriptable scope, Scriptable holdable, Object[] objects){
	        return objects[1];
	    }
	}
	
	Context mContext;
	Scriptable 	mScope;
	Function mMainCallback;
	
	public MDSJS(Context zContext, Scriptable zScope) {
		mContext 	= zContext;
		mScope 		= zScope;
	}
	
	public void callMainCallback() {
		
//		JSONObject json = new JSONObject();
//		json.put("status", true);
//		Object[] args = { json };
//		
//		mMainCallback.call(mContext, mScope, mScope, args);
	}
	
	public void init(Function zCallback) {
		
		mMainCallback = zCallback;
		
		MinimaLogger.log("MDS INIT CALLED");
		
		JSONObject json = new JSONObject();
		json.put("status", true);
		
		Object nativejson = NativeJSON.parse(mContext, mScope, json.toString(), new NullCallable());

		Object[] args = { nativejson };
		
		zCallback.call(mContext, mScope, mScope, args);
	}
	
}
