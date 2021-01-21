package org.minima.kissvm.values;

import org.minima.kissvm.Contract;
import org.minima.objects.base.MiniString;

public class ScriptValue extends HEXValue {
	
	/**
	 * The Script
	 */
	String mScript;
	
	public ScriptValue(String zScript) {
		//Remove the bracket and space at the beginning and end
		super(Contract.cleanScript(zScript).getBytes(MiniString.MINIMA_CHARSET));

		mScript = new String( getRawData(), MiniString.MINIMA_CHARSET );
	}
	
	@Override
	public String toString() {
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
		return new ScriptValue(mScript+" "+zSCValue.toString());
	}
}
