package org.minima.kissvm.values;

import org.minima.objects.base.MiniString;

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
	
	public byte[] getBytes(){
		return mScript.getBytes(MiniString.MINIMA_CHARSET);
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
