/**
 * 
 */
package org.minima.kissvm.expressions;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class BooleanExpression implements Expression {

	public static final int BOOLEAN_AND    = 0;
	public static final int BOOLEAN_NAND   = 1;
	public static final int BOOLEAN_OR     = 2;
	public static final int BOOLEAN_NOR    = 3;
	public static final int BOOLEAN_XOR    = 4;
	public static final int BOOLEAN_NXOR   = 5;
	
	public static final int BOOLEAN_EQ     = 6;
	public static final int BOOLEAN_NEQ    = 7;
	public static final int BOOLEAN_LT     = 8;
	public static final int BOOLEAN_LTE    = 9;
	public static final int BOOLEAN_GT     = 10;
	public static final int BOOLEAN_GTE    = 11;
	
	public static final int BOOLEAN_NOT    = 12;
	
	int mBooleanType;
	
	Expression mLeft;
	Expression mRight;
	
	/**
	 * Used for NOT
	 * @param zLeft
	 * @param zBooleanType
	 */
	public BooleanExpression( Expression zLeft, int zBooleanType) {
		this(zLeft, new ConstantExpression(BooleanValue.FALSE) , zBooleanType);
	}
	
	public BooleanExpression( Expression zLeft, Expression zRight , int zBooleanType) {
		mLeft 			= zLeft;
		mRight 			= zRight;
		mBooleanType	= zBooleanType;
	}
	
	public BooleanValue getBooleanValue(Contract zContract) throws ExecutionException {
		return (BooleanValue)getValue(zContract);
	}
	
	@Override
	public Value getValue(Contract zContract) throws ExecutionException {
		Value ret = null;
		
		//This action counts as one instruction
		zContract.incrementInstructions();
				
		//Calculate the left and the right side
		Value lval = mLeft.getValue(zContract);
		Value rval = mRight.getValue(zContract);

		//Only check for double value expressions
		if(mBooleanType != BOOLEAN_NOT) {
			//Make sure both Values are of the same type
			Value.checkSameType(lval, rval);
		}
		
		boolean left,right;
		switch(mBooleanType) {
		
				/**
				 * ONLY Works for BOOLEAN value types
				 */
			case BOOLEAN_AND :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				right = ((BooleanValue)rval).isTrue();
				
				ret = left && right ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NAND :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				right = ((BooleanValue)rval).isTrue();
				
				ret = left && right ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
			case BOOLEAN_OR :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				right = ((BooleanValue)rval).isTrue();
				
				ret = left || right ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NOR :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				right = ((BooleanValue)rval).isTrue();
				
				ret = left || right ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
			
			case BOOLEAN_XOR :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				right = ((BooleanValue)rval).isTrue();
				
				ret = left ^ right ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NXOR :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				right = ((BooleanValue)rval).isTrue();
				
				ret = left ^ right ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
			case BOOLEAN_NOT :
				lval.verifyType(Value.VALUE_BOOLEAN);
				left  = ((BooleanValue)lval).isTrue();
				
				ret = left ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
			
				
				/**
				 * Works for ANY value types
				 */
			case BOOLEAN_EQ :
			{
				if(lval.getValueType() == Value.VALUE_BOOLEAN) {
					left  = ((BooleanValue)lval).isTrue();
					right = ((BooleanValue)rval).isTrue();
					ret = left == right ? BooleanValue.TRUE : BooleanValue.FALSE;
				
				}else if(lval.getValueType() == Value.VALUE_HEX) {
					HexValue lefthex  = ((HexValue)lval);
					HexValue righthex  = ((HexValue)rval);
					ret = lefthex.isEqual(righthex) ? BooleanValue.TRUE : BooleanValue.FALSE;
				
				}else if(lval.getValueType() == Value.VALUE_NUMBER) {
					NumberValue leftnum  = ((NumberValue)lval);
					NumberValue rightnum  = ((NumberValue)rval);
					ret = leftnum.isEqual(rightnum) ? BooleanValue.TRUE : BooleanValue.FALSE;
				
				}else if(lval.getValueType() == Value.VALUE_SCRIPT) {
					StringValue leftstr   = ((StringValue)lval);
					StringValue rightstr  = ((StringValue)rval);
					ret = leftstr.isEqual(rightstr) ? BooleanValue.TRUE : BooleanValue.FALSE;
				
				}
				
				break;
			}
			case BOOLEAN_NEQ :
			{
				if(lval.getValueType() == Value.VALUE_BOOLEAN) {
					left  = ((BooleanValue)lval).isTrue();
					right = ((BooleanValue)rval).isTrue();
					ret = left == right ? BooleanValue.FALSE : BooleanValue.TRUE;
				
				}else if(lval.getValueType() == Value.VALUE_HEX) {
					HexValue lefthex  = ((HexValue)lval);
					HexValue righthex  = ((HexValue)rval);
					ret = lefthex.isEqual(righthex) ? BooleanValue.FALSE : BooleanValue.TRUE;
				
				}else if(lval.getValueType() == Value.VALUE_NUMBER) {
					NumberValue leftnum  = ((NumberValue)lval);
					NumberValue rightnum  = ((NumberValue)rval);
					ret = leftnum.isEqual(rightnum) ? BooleanValue.FALSE : BooleanValue.TRUE;
				
				}else if(lval.getValueType() == Value.VALUE_SCRIPT) {
					StringValue leftstr   = ((StringValue)lval);
					StringValue rightstr  = ((StringValue)rval);
					ret = leftstr.isEqual(rightstr) ? BooleanValue.FALSE : BooleanValue.TRUE;
				
				}
				
				break;
			}
			
				/**
				 * ONLY works for NUMBER value types
				 */
			case BOOLEAN_LT :
			{
				lval.verifyType(Value.VALUE_NUMBER);
				NumberValue leftnum  = ((NumberValue)lval);
				NumberValue rightnum  = ((NumberValue)rval);
				ret = leftnum.isLess(rightnum) ? BooleanValue.TRUE : BooleanValue.FALSE;
			
				break;
			}
			case BOOLEAN_LTE :
			{
				lval.verifyType(Value.VALUE_NUMBER);
				NumberValue leftnum  = ((NumberValue)lval);
				NumberValue rightnum  = ((NumberValue)rval);
				ret = leftnum.isLessEqual(rightnum) ? BooleanValue.TRUE : BooleanValue.FALSE;
			
				break;
			}	
			case BOOLEAN_GT :
			{
				lval.verifyType(Value.VALUE_NUMBER);
				NumberValue leftnum  = ((NumberValue)lval);
				NumberValue rightnum  = ((NumberValue)rval);
				ret = leftnum.isMore(rightnum) ? BooleanValue.TRUE : BooleanValue.FALSE;
			
				break;
			}
			case BOOLEAN_GTE :
			{
				lval.verifyType(Value.VALUE_NUMBER);
				NumberValue leftnum  = ((NumberValue)lval);
				NumberValue rightnum  = ((NumberValue)rval);
				ret = leftnum.isMoreEqual(rightnum) ? BooleanValue.TRUE : BooleanValue.FALSE;
			
				break;
			}
				
			default :
				throw new ExecutionException("UNKNOWN boolean operator : "+mBooleanType);	
		}
				
		//And trace it..
		zContract.traceLog(toString()+" returns:"+ret.toString());
				
		return ret;
	}

	@Override
	public String toString() {
		String ret = "ERROR";
		
		switch (mBooleanType) {
			case BOOLEAN_AND :
				ret = "AND";
				break;
			case BOOLEAN_NAND :
				ret = "NAND";
				break;
			case BOOLEAN_OR:
				ret = "OR";
				break;
			case BOOLEAN_NOR:
				ret = "NOR";
				break;
			case BOOLEAN_XOR :
				ret = "XOR";
				break;
			case BOOLEAN_NXOR :
				ret = "NXOR";
				break;
			
			case BOOLEAN_EQ :
				ret = "EQ";
				break;
			case BOOLEAN_NEQ :
				ret = "NEQ";
				break;
			
			case BOOLEAN_LT :
				ret = "LT";
				break;
			case BOOLEAN_LTE :
				ret = "LTE";
				break;
			
			case BOOLEAN_GT :
				ret = "GT";
				break;
			case BOOLEAN_GTE :
				ret = "GTE";
				break;
			
			case BOOLEAN_NOT :
				return "NOT ( "+mLeft+" )";
		}
		
		return "( "+mLeft+" "+ret+" "+mRight+" )";
	}
}
