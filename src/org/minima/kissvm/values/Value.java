/**
 * 
 */
package org.minima.kissvm.values;

import java.math.BigInteger;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.tokens.Token;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

/**
 * @author Spartacus Rex
 *
 */
public abstract class Value {
	
	/**
	 * The Numeric Value
	 */
	protected MiniNumber mNumber;
	
	/**
	 * The RAW bytes
	 */
	protected MiniData mData;
	
	/**
	 * What typ of Value is this..
	 * @return
	 */
	public abstract int getValueType();
	
	/**
	 * The Number Version
	 * @return
	 */
	public MiniNumber getNumber() {
		return mNumber;
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
		return getMiniData().getData();
	}
	
	/**
	 * Boolean Operators
	 */
	public abstract boolean isEqual(Value zValue);	
	public abstract boolean isLess(Value zValue);
	public abstract boolean isLessEqual(Value zValue);
	public abstract boolean isMore(Value zValue);
	public abstract boolean isMoreEqual(Value zValue);
	
	public boolean isTrue() {
		return !isFalse();
	}
	
	public boolean isFalse() {
		return mNumber.isEqual(MiniNumber.ZERO);
	}
	
	/**
	 * Operators
	 */
	public abstract Value add(Value zValue);	
	public abstract Value sub(Value zValue);	
	public abstract Value mult(Value zValue);	
	public abstract Value div(Value zValue);	
	
	/**
	 * GLOBAL STATIC FUNCTION for creating a Value from any string
	 */
	public static Value getValue(String zValue){
		if(zValue.startsWith("[")) {
			//First remove the brackets
			String sc = zValue.substring(1,zValue.length()-1);
			
			//Then initialise the value 
			return new ScriptValue(sc);
			
		}else if(zValue.startsWith("0x")) {
			return new HEXValue(zValue);

		}else if(zValue.equals("TRUE")) {
			return BooleanValue.TRUE;

		}else if(zValue.equals("FALSE")) {
			return BooleanValue.FALSE;

		}else {
			return new NumberValue(zValue);
		}
	}
	
	/**
	 * GLOBAL STATIC FUNCTION for telling the value type
	 */
	public static int getValueType(String zValue){
		if(zValue.startsWith("[")) {
			//Then initialise the value 
			return ScriptValue.VALUE_SCRIPT;
			
		}else if(zValue.startsWith("0x")) {
			return HEXValue.VALUE_HEX;

		}else if(zValue.equals("TRUE")) {
			return BooleanValue.VALUE_BOOLEAN;

		}else if(zValue.equals("FALSE")) {
			return BooleanValue.VALUE_BOOLEAN;

		}else if(Token.isNumeric(zValue)){
			return NumberValue.VALUE_NUMBER;
	
		}else {
			//ERROR
			return -99;
		}
	}
	
	/**
	 * Test Program
	 */
	public static void main(String[] zArgs) {
		Value vv = new HEXValue ("0xF184A0A4295AD508CEC610C7430CE328F184A0A4295AD508CEC610C7430CE328");
		
		MiniNumber q = new MiniNumber("1");
		MiniNumber e = new MiniNumber("3");
		MiniNumber t = q.div(e);
		
		System.out.println(vv);
		
//		//Get the data..
//		BigInteger bb = vv.getRamNumber().getAsBigInteger();
//		BigInteger cc = bb.shiftRight(2);
//		
//		Value v2 = new Value (cc.toString());
//		System.out.println(v2.mNumber);
		
	}	
}
