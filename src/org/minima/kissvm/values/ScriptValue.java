package org.minima.kissvm.values;

import java.nio.charset.Charset;

import org.minima.kissvm.Contract;
import org.minima.objects.base.MiniData;

public class ScriptValue extends HEXValue {
	
	/**
	 * The Script
	 */
	String mScript;
	
	/**
	 * Initialise a Clean Script Value
	 * Convert the internal data straight to hex
	 * All scripts MUST be cleaned before & start with 
	 * bracket space and end with bracket space..
	 * @param zString
	 */
	public ScriptValue(String zScript) {
		super();
		
		//Trim it..
		String scr = zScript.trim();
		
		//Check the script starts and ends with []
		if(!scr.startsWith("[") || !scr.endsWith("]")) {
			throw new IllegalArgumentException("ScriptValue MUST start with [ and end with ]");
		}
		
		//Clean the script
		String cscr = Contract.cleanScript(scr);
		
		//Do not store the Brackets..
		String finalscr = cscr.substring(2, cscr.length()-2);
		
		//Now set the data
		byte[] data = finalscr.getBytes(Charset.forName("US-ASCII"));
		
		//Set the parent MiniData
		mData = new MiniData(data);
	
		//And store for later
		mScript = new String( data, Charset.forName("US-ASCII") );
	}
	
	@Override
	public String toString() {
		return "[ "+mScript+" ]";
	}
	
	/**
	 * Return the script part with out the brackets
	 * 
	 * @return
	 */
	public String getScriptOnly() {
		return mScript;
	}
	
	@Override
	public int getValueType() {
		return VALUE_SCRIPT;
	}
	
	/**
	 * Add this script and return the result
	 * 
	 * @param zSCValue
	 * @return
	 */
	public ScriptValue add(ScriptValue zSCValue) {
		return new ScriptValue("[ "+mScript+" "+zSCValue.getScriptOnly()+" ]");
	}
}
