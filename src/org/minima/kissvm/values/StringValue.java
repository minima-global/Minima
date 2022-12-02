package org.minima.kissvm.values;

import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.objects.base.MiniString;

public class StringValue extends Value {
	
	/**
	 * MAX String Value length is 1MB - can still use multiple MAST scripts for more..
	 */
	public static final int MAX_STRING_LEN = 1024 * 64;
	
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
	
	public MiniString getMiniString() {
		return mScript;
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
	
	public void checkSize() throws ExecutionException {
		int len = getBytes().length;
		if(len > MAX_STRING_LEN) {
			throw new ExecutionException("MAX String length reached (64K max) : "+len);
		}
	}
}
