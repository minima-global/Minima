package org.minima.kissvm.values;

public class BooleanValue extends Value {
	
	/**
	 * Global True False..
	 */
	public static final BooleanValue FALSE = new BooleanValue(false);
	public static final BooleanValue TRUE  = new BooleanValue(true);
	
	boolean mTrue;
	
	public BooleanValue(boolean zValue) {
		mTrue = zValue;
	}
	
	/**
	 * TRUE or FALSE
	 */
	public boolean isTrue() {
		return mTrue;
	}
	
	public boolean isFalse() {
		return !mTrue;
	}
	
	@Override
	public String toString() {
		return (isTrue() ? "TRUE" : "FALSE");
	}
	
	@Override
	public int getValueType() {
		return VALUE_BOOLEAN;
	}
	
	public boolean isEqual(BooleanValue zBool) {
		return mTrue == zBool.isTrue();
	}
}
