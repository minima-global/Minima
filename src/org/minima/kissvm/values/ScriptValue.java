package org.minima.kissvm.values;

import java.nio.charset.Charset;

import org.minima.kissvm.Contract;

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
		//Remove the bracket and space at the beginning and end
		super(Contract.cleanScript(zScript).getBytes(Charset.forName("US-ASCII")));
		
		//And store for later
		mScript = new String( getRawData(), Charset.forName("US-ASCII") );
	}
	
	@Override
	public String toString() {
		return mScript;
	}
	
	@Override
	public int getValueType() {
		return VALUE_SCRIPT;
	}
	
	public Value add(ScriptValue zValue) {
		String sum  = mScript + " " + zValue.toString();
		return new ScriptValue( sum );
	}
}
