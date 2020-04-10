/**
 * 
 */
package org.minima.kissvm.expressions;

import java.math.BigInteger;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

/**
 * @author Spartacus Rex
 *
 */
public class OperatorExpression implements Expression{

	public static final int OPERATOR_ADD   = 0;
	public static final int OPERATOR_SUB   = 1;
	public static final int OPERATOR_MUL   = 2;
	public static final int OPERATOR_DIV   = 3;
	
	public static final int OPERATOR_NEG   = 4;
	
	public static final int OPERATOR_SHIFTL   = 5;
	public static final int OPERATOR_SHIFTR   = 6;
	
	public static final int OPERATOR_MODULO   = 7;
	
	public static final int OPERATOR_AND   		= 8;
	public static final int OPERATOR_OR   		= 9;
	public static final int OPERATOR_XOR   		= 10;
	
	
	int mOperatorType;
	
	Expression mLeft;
	Expression mRight;
	
	public OperatorExpression( Expression zLeft, int zOperator) {
		this(zLeft, new ConstantExpression(BooleanValue.FALSE), zOperator);
	}
	
	public OperatorExpression( Expression zLeft, Expression zRight , int zOperator) {
		mLeft 			= zLeft;
		mRight 			= zRight;
		mOperatorType 	= zOperator;
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.expressions.Expression#getValue(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value getValue(Contract zContract) throws ExecutionException {
		//This action counts as one instruction
		zContract.incrementInstructions();
				
		Value ret = null;
		
		Value lval = mLeft.getValue(zContract);
		Value rval = mRight.getValue(zContract);
		
		MiniNumber left  = lval.getNumber();
		MiniNumber right = rval.getNumber();
		
		MiniData ldata  = lval.getMiniData();
		MiniData rdata  = rval.getMiniData();
		
		BigInteger lbig = ldata.getDataValue();
		BigInteger rbig = rdata.getDataValue();
		
		//Which Operator..
		switch (mOperatorType) {
		case OPERATOR_ADD :
			ret = lval.add(rval);
			break;
		case OPERATOR_SUB :
			ret = lval.sub(rval);
			break;
		case OPERATOR_MUL :
			ret = lval.mult(rval);
			break;
		case OPERATOR_DIV :
			//Check Divide by zero
			if(right.isEqual(MiniNumber.ZERO)) {
				throw new ExecutionException("Divide By ZERO! "+toString()); 
			}
			
			ret = lval.div(rval);
			
			break;
		case OPERATOR_NEG :
			ret = new NumberValue( left.mult(MiniNumber.MINUSONE) );
			break;
		
		case OPERATOR_MODULO :
			ret = new NumberValue( left.modulo(right) );
			break;
		
			/**
			 * These are done on the RAW data - not the number
			 */
		case OPERATOR_SHIFTL :
			ret = new HEXValue( ldata.shiftl(right.getAsInt()).toString() );
			break;
		case OPERATOR_SHIFTR :
			ret = new HEXValue( ldata.shiftr(right.getAsInt()).toString() );
			break;
		
			/**
			 * Bitwise operators - done on the RAW data - not the number
			 */
		case OPERATOR_AND :
			ret = new HEXValue ( lbig.and(rbig).toString(16) );
			break;
		case OPERATOR_OR :
			ret = new HEXValue ( lbig.or(rbig).toString(16) );
			break;
		case OPERATOR_XOR :
			ret = new HEXValue ( lbig.xor(rbig).toString(16) );
			break;
			
		}
		
		//And trace it..
		zContract.traceLog(toString()+" returns:"+ret.toString());
				
		return ret;
	}
	
	@Override
	public String toString() {
		String ret = "ERROR";
		
		//Which Operator..
		switch (mOperatorType) {
		case OPERATOR_ADD :
			ret = "+";
			break;
		
		case OPERATOR_SUB :
			ret = "-";
			break;
		
		case OPERATOR_MUL :
			ret = "*";
			break;
		
		case OPERATOR_DIV :
			ret = "/";
			break;
		
		case OPERATOR_MODULO :
			ret = "%";
			break;
		
		case OPERATOR_SHIFTL :
			ret = "<<";
			break;
		
		case OPERATOR_SHIFTR:
			ret = ">>";
			break;
		
		case OPERATOR_NEG :
			return " - ( "+mLeft+" )";
		
		case OPERATOR_AND :
			ret = "&";
			break;
		case OPERATOR_OR :
			ret = "|";
			break;
		case OPERATOR_XOR :
			ret = "^";
			break;
		}
		
		return "( "+mLeft + " "+ret+" " + mRight+" )";
	}
	
}
