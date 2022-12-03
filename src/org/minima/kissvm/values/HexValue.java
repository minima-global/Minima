package org.minima.kissvm.values;

import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class HexValue extends Value {
	
	/**
	 * Maximum size of a HEX value - 256K
	 */
	public static final int MAX_HEX_SIZE = 256 * 1024;
	
	/**
	 * The RAW bytes
	 */
	protected MiniData mData;
	
	/**
	 * Convert a HEX String into a byte array
	 */
	public HexValue(String zHex) {
		this(new MiniData(zHex));
	}
	
	/**
	 * Convert a MiniData byte array into a HEXValue
	 */
	public HexValue(MiniData zData) {
		this(zData.getBytes()); 
	}
	
	/**
	 * Convert a byte array into a HEXValue
	 */
	public HexValue(byte[] zData) {
		//It's a HEX value..
		mData   = new MiniData(zData);
		
		//Check Size
		int len = mData.getLength();
		if(len > MAX_HEX_SIZE) {
			throw new IllegalArgumentException("MAX HEX value size reached : "+len+" MAX:"+MAX_HEX_SIZE);
		}
	}
	
	/**
	 * Convert a positive whole number into a HEXValue..
	 * 
	 * If the number is not positive or not a whole number exception thrown
	 * 
	 * @param zNumber
	 */
	public HexValue(MiniNumber zNumber) {
		if(zNumber.isLess(MiniNumber.ZERO)){
			throw new NumberFormatException("HEXValue Number must be positive");
		}
		
		if(!zNumber.floor().isEqual(zNumber)){
			throw new NumberFormatException("HEXValue Number must be a whole number");
		}
		
		//HEX
		mData 	= new MiniData(zNumber.getAsBigInteger().toByteArray());
	}
	
	/**
	 * The Data version
	 * @return
	 */
	public MiniData getMiniData() {
		return mData;
	}
	
	/**
	 * Get the RAW byte data
	 * @return
	 */
	public byte[] getRawData() {
		return getMiniData().getBytes();
	}
	
	@Override
	public int getValueType() {
		return VALUE_HEX;
	}
	
	public boolean isEqual(HexValue zValue) {
		return mData.isEqual(zValue.getMiniData());
	}
	
	@Override
	public String toString() {
		return mData.toString();
	}
}
