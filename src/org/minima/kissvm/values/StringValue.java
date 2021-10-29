package org.minima.kissvm.values;

import org.minima.objects.base.MiniString;

public class StringValue extends Value {
	
	/**
	 * The String
	 */
	MiniString mScript;
	
	public StringValue(String zScript) {
		mScript = new MiniString( zScript );
	}
	
	@Override
	public String toString() {
		return mScript.toString();
	}
	
	public byte[] getBytes(){
		return mScript.getData();
	}
	
	@Override
	public int getValueType() {
		return VALUE_SCRIPT;
	}
	
	public boolean isEqual(StringValue zValue) {
		return mScript.toString().equals(zValue.toString());
	}
	
	public StringValue add(StringValue zSCValue) {
		return new StringValue(mScript.toString()+zSCValue.toString());
	}
}
