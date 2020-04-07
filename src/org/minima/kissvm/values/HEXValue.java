package org.minima.kissvm.values;

import java.math.BigInteger;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class HEXValue extends Value {

	public static final int VALUE_HEX = 1;
	
	public HEXValue(MiniData zData) {
		this(zData.getData()); 
	}
	
	public HEXValue(byte[] zData) {
		//It's a HEX value..
		mData   = new MiniData(zData);
		
		//And now
		mNumber = new MiniNumber(mData.getDataValue());
	}
	
	public HEXValue(MiniNumber zNumber) {
		this(new BigInteger(zNumber.toString()).toString(16).toUpperCase());
	}
	
	public HEXValue(String zHex) {
		//HEX
		mData 	= new MiniData(zHex);
		
		//THE NUMBER is only to 128 BIT (not complete). The full BigInteger value is stored in the RamData
		mNumber = new MiniNumber(mData.getDataValue());
	}
	
	@Override
	public int getValueType() {
		return VALUE_HEX;
	}
	
	@Override
	public boolean isEqual(Value zValue) {
		return mData.isEqual(zValue.getMiniData());
	}
	
	@Override
	public boolean isLess(Value zValue) {
		return mData.isLess(zValue.getMiniData());
	}
	
	@Override
	public boolean isLessEqual(Value zValue) {
		return mData.isLessEqual(zValue.getMiniData());
	}
	
	@Override
	public boolean isMore(Value zValue) {
		return mData.isMore(zValue.getMiniData());
	}
	
	@Override
	public boolean isMoreEqual(Value zValue) {
		return mData.isMoreEqual(zValue.getMiniData());
	}
	
	@Override
	public Value add(Value zValue) {
		return new HEXValue( mNumber.add(zValue.getNumber()) );
	}
	
	@Override
	public Value sub(Value zValue) {
		return new HEXValue( mNumber.sub(zValue.getNumber()) );
	}
	
	@Override
	public Value mult(Value zValue) {
		return new HEXValue( mNumber.mult(zValue.getNumber()) );
	}
	
	@Override
	public Value div(Value zValue) {
		return new HEXValue( mNumber.div(zValue.getNumber()) );
	}
	
	@Override
	public String toString() {
		return mData.toString();
	}
}
