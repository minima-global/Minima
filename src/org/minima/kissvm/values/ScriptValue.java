package org.minima.kissvm.values;

public class ScriptValue extends Value {
	
	/**
	 * The Script
	 */
	String mScript;
	
	public ScriptValue(String zScript) {
		mScript = new String( zScript );
	}
	
	@Override
	public String toString() {
		return mScript;
	}
	
	@Override
	public int getValueType() {
		return VALUE_SCRIPT;
	}
	
	public boolean isEqual(ScriptValue zValue) {
		return mScript.equals(zValue.toString());
	}
	
	/**
	 * Add this script and return the result
	 * 
	 * @param zSCValue
	 * @return
	 */
	public ScriptValue add(ScriptValue zSCValue) {
		return new ScriptValue(mScript+zSCValue.toString());
	}
}
