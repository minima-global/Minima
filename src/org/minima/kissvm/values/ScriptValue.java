package org.minima.kissvm.values;

import java.nio.charset.Charset;

import org.minima.kissvm.Contract;

public class ScriptValue extends HEXValue {
	
	public static final int VALUE_SCRIPT = 3;
	
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
	
	@Override
	public Value add(Value zValue) {
		String sum  = mScript + " " + new String( zValue.getRawData(), Charset.forName("US-ASCII") );
		return new ScriptValue( sum );
	}
}
