package org.minima.kissvm.values;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class NumberValue extends Value {

	public static final int VALUE_NUMBER = 2;
	
	public NumberValue(int zValue) {
		this(Integer.toString(zValue));
	}
	
	public NumberValue(long zValue) {
		this(Long.toString(zValue));
	}
	
	public NumberValue(double zValue) {
		this(Double.toString(zValue));
	}
	
	public NumberValue(MiniNumber zValue) {
		this(zValue.toString());
	}
	
	public NumberValue(String zNumber) {
		//NUMERIC
		mNumber = new MiniNumber(zNumber);
	
		//The raw data is just the bytes of the string version.. at least unique for the input
		mData = MiniData.getMiniDataVersion(mNumber);
	}
	
	@Override
	public int getValueType() {
		return VALUE_NUMBER;
	}
	
	@Override
	public boolean isEqual(Value zValue) {
		return mNumber.isEqual(zValue.getNumber());
	}
	
	@Override
	public boolean isLess(Value zValue) {
		return mNumber.isLess(zValue.getNumber());
	}
	
	@Override
	public boolean isLessEqual(Value zValue) {
		return mNumber.isLessEqual(zValue.getNumber());
	}
	
	@Override
	public boolean isMore(Value zValue) {
		return mNumber.isMore(zValue.getNumber());
	}
	
	@Override
	public boolean isMoreEqual(Value zValue) {
		return mNumber.isMoreEqual(zValue.getNumber());
	}
	
	@Override
	public Value add(Value zValue) {
		return new NumberValue( mNumber.add(zValue.getNumber()) );
	}
	
	@Override
	public Value sub(Value zValue) {
		return new NumberValue( mNumber.sub(zValue.getNumber()) );
	}
	
	@Override
	public Value mult(Value zValue) {
		return new NumberValue( mNumber.mult(zValue.getNumber()) );
	}
	
	@Override
	public Value div(Value zValue) {
		return new NumberValue( mNumber.div(zValue.getNumber()) );
	}
	
	@Override
	public String toString() {
		return mNumber.toString();
	}
}
