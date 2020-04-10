/**
 * 
 */
package org.minima.kissvm.expressions;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.BooleanValue;
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
	
	@Override
	public Value getValue(Contract zContract) throws ExecutionException {
		//This action counts as one instruction
		zContract.incrementInstructions();
		
		Value ret = null;
		
		Value lval = mLeft.getValue(zContract);
		Value rval = mRight.getValue(zContract);
				
		boolean left   	= lval.isTrue(); 
		boolean right  	= rval.isTrue();
		
		switch(mBooleanType) {
			case BOOLEAN_AND :
				ret = left && right ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NAND :
				ret = left && right ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
				
			case BOOLEAN_OR :
				ret = left || right ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NOR :
				ret = left || right ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
			
			case BOOLEAN_XOR :
				ret = left ^ right ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NXOR :
				ret = left ^ right ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
				
			case BOOLEAN_EQ :
				ret = lval.isEqual(rval) ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_NEQ :
				ret = lval.isEqual(rval) ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;
			
			case BOOLEAN_LT :
				ret = lval.isLess(rval) ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_LTE :
				ret = lval.isLessEqual(rval) ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			
			case BOOLEAN_GT :
				ret = lval.isMore(rval) ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			case BOOLEAN_GTE :
				ret = lval.isMoreEqual(rval) ? BooleanValue.TRUE : BooleanValue.FALSE;
				break;
			
			case BOOLEAN_NOT :
				ret = lval.isTrue() ? BooleanValue.FALSE : BooleanValue.TRUE;
				break;	
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
