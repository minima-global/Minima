package org.minima.kissvm.values;

import org.minima.objects.base.MiniNumber;

public class NumberValue extends Value {
	
	/**
	 * The MiniNumber Numeric Value
	 */
	protected MiniNumber mNumber;
	
	public NumberValue(int zValue) {
		this(Integer.toString(zValue));
	}
	
	public NumberValue(long zValue) {
		this(Long.toString(zValue));
	}
	
	public NumberValue(MiniNumber zValue) {
		mNumber = zValue;
	}
	
	public NumberValue(String zNumber) {
		mNumber = new MiniNumber(zNumber);
	}
	
	/**
	 * The Number Version
	 * @return
	 */
	public MiniNumber getNumber() {
		return mNumber;
	}
	
	@Override
	public int getValueType() {
		return VALUE_NUMBER;
	}
	
	public boolean isFalse() {
		return mNumber.isEqual(MiniNumber.ZERO);
	}
	
	public boolean isEqual(NumberValue zValue) {
		return mNumber.isEqual(zValue.getNumber());
	}
	
	public boolean isLess(NumberValue zValue) {
		return mNumber.isLess(zValue.getNumber());
	}
	
	public boolean isLessEqual(NumberValue zValue) {
		return mNumber.isLessEqual(zValue.getNumber());
	}
	
	public boolean isMore(NumberValue zValue) {
		return mNumber.isMore(zValue.getNumber());
	}
	
	public boolean isMoreEqual(NumberValue zValue) {
		return mNumber.isMoreEqual(zValue.getNumber());
	}
	
	public NumberValue add(NumberValue zValue) {
		return new NumberValue( mNumber.add(zValue.getNumber()) );
	}
	
	public NumberValue sub(NumberValue zValue) {
		return new NumberValue( mNumber.sub(zValue.getNumber()) );
	}
	
	public NumberValue mult(NumberValue zValue) {
		return new NumberValue( mNumber.mult(zValue.getNumber()) );
	}
	
	public NumberValue div(NumberValue zValue) {
		return new NumberValue( mNumber.div(zValue.getNumber()) );
	}
	
	@Override
	public String toString() {
		return mNumber.toString();
	}
}
