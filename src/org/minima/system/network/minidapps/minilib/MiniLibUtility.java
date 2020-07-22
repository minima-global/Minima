package org.minima.system.network.minidapps.minilib;

import java.util.Map.Entry;
import java.util.Set;

import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Callable;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.NativeArray;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.NativeObject;
import org.mozilla.javascript.Scriptable;

public class MiniLibUtility {

	public static Object makeJSONObject(String zJSON, Context rhino, Scriptable scope) {
	    Object param = NativeJSON.parse(rhino, scope, zJSON, new Callable() {
			@Override
			public Object call(Context arg0, Scriptable arg1, Scriptable arg2, Object[] arg3) {
				return arg3[1];
			}
		});
	    return param;
	}
	
	public static JSONObject toJsonObject(NativeObject nativeObject){
		
        JSONObject object = new JSONObject();
        Set<Entry<Object, Object>> entrySet = nativeObject.entrySet();
        for (Entry<Object, Object> entry : entrySet){
            if (entry.getValue() instanceof String){
                object.put(entry.getKey().toString(), entry.getValue().toString());
            
        	}else if (entry.getValue() instanceof Integer){
                object.put(entry.getKey().toString(), ((Integer)entry.getValue()).intValue() );
            
        	}else if (entry.getValue() instanceof Long ){
                object.put(entry.getKey().toString(), ((Long)entry.getValue()).longValue() );
        	
        	}else if (entry.getValue() instanceof Double ){
                object.put(entry.getKey().toString(), ((Double)entry.getValue()).doubleValue() );
        	
        	}else if (entry.getValue() instanceof Boolean ){
        		object.put(entry.getKey().toString(), ((Boolean)entry.getValue()).booleanValue() );
            
        	}else if (entry.getValue() instanceof NativeArray){
                object.put(entry.getKey().toString(), toJsonArray((NativeArray) entry.getValue()));
            
        	}else if (entry.getValue() instanceof NativeObject){
                object.put(entry.getKey().toString(), toJsonObject((NativeObject) entry.getValue()));
            
        	}else {
            	MinimaLogger.log("UNKNOWN JSON Object type : "+entry.getValue().getClass()+" "+entry.getValue().toString());
            }
        }
        
        return object;
    }

    public static JSONArray toJsonArray(NativeArray nativeArray){
        JSONArray array = new JSONArray();
        for (Object obj : nativeArray){
            if (obj instanceof NativeObject){
                array.add(toJsonObject((NativeObject) obj));
           
            }else if (obj instanceof NativeArray){
                array.add(toJsonArray((NativeArray) obj));
            
            }else if (obj instanceof String){
                array.add((String)obj);
            
            }else if (obj instanceof Integer){
                array.add(((Integer)obj).intValue());
            
            }else if (obj instanceof Long){
                array.add(((Long)obj).longValue());
            
            }else if (obj instanceof Double){
                array.add(((Double)obj).doubleValue());
            
            }else if (obj instanceof Boolean){
                array.add(((Boolean)obj).booleanValue());
            
            }else{
            	MinimaLogger.log("UNKNOWN JSONARRAY Object : "+obj.getClass()+" "+obj.toString());
            }
        }

        return array;
    }

}
