package org.minima.kissvm.values;

public class BooleanValue extends NumberValue {

	public static final int VALUE_BOOLEAN = 4;
	
	/**
	 * Global True False..
	 */
	public static final BooleanValue FALSE = new BooleanValue(false);
	public static final BooleanValue TRUE  = new BooleanValue(true);
	
	public BooleanValue(boolean zValue) {
		super(zValue ? 1 : 0);
	}
	
	@Override
	public String toString() {
		return (isTrue() ? "TRUE" : "FALSE");
	}
	
	@Override
	public int getValueType() {
		return VALUE_BOOLEAN;
	}
}
