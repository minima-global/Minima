/**
 * 
 */
package org.minima.kissvm.expressions;

import java.math.BigInteger;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
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
		Value ret = null;
		
		//This action counts as one instruction
		zContract.incrementInstructions();
				
		Value lval = mLeft.getValue(zContract);
		Value rval = mRight.getValue(zContract);
						
		//Which Operator..
		switch (mOperatorType) {
		
			/**
			 * ADD is SPECIAL
			 * 
			 * Works with Number OR SCRIPT
			 */
		case OPERATOR_ADD :
			{
				if(lval.getValueType() == Value.VALUE_NUMBER) {
					Value.checkSameType(lval, rval);
					NumberValue lnv = (NumberValue)lval;
					NumberValue rnv = (NumberValue)rval;
					MiniNumber ans = lnv.getNumber().add(rnv.getNumber());
					return new NumberValue(ans);
					
				}else if(lval.getValueType() == Value.VALUE_SCRIPT) {
					Value.checkSameType(lval, rval);
					StringValue lnv = (StringValue)lval;
					StringValue rnv = (StringValue)rval;
					return lnv.add(rnv);
				
				}else {
					throw new ExecutionException("Invalid type in ADD. MUST be Number or String "+lval.toString());
				}
			}
			
			
			/**
			 * MUST be NUMBERS
			 */
		case OPERATOR_SUB :
			{
				Value.checkSameType(lval, rval,Value.VALUE_NUMBER);
				NumberValue lnv = (NumberValue)lval;
				NumberValue rnv = (NumberValue)rval;
				ret = lnv.sub(rnv);
			}
			break;
			
		case OPERATOR_MUL :
			{
				Value.checkSameType(lval, rval,Value.VALUE_NUMBER);
				NumberValue lnv = (NumberValue)lval;
				NumberValue rnv = (NumberValue)rval;
				ret = lnv.mult(rnv);
			}
			break;
		case OPERATOR_DIV :
			{
				Value.checkSameType(lval, rval,Value.VALUE_NUMBER);
				NumberValue lnv = (NumberValue)lval;
				NumberValue rnv = (NumberValue)rval;
				if(rnv.getNumber().isEqual(MiniNumber.ZERO)) {
					throw new ExecutionException("Divide By ZERO! "+toString()); 
				}
				ret = lnv.div(rnv);
			}
			break;
		
		case OPERATOR_NEG :
			{
				lval.verifyType(Value.VALUE_NUMBER);
				NumberValue lnv = (NumberValue)lval;
				ret = new NumberValue( lnv.getNumber().mult(MiniNumber.MINUSONE) );
			}
			break;
		
		case OPERATOR_MODULO :
			{
				Value.checkSameType(lval, rval,Value.VALUE_NUMBER);
				NumberValue lnv = (NumberValue)lval;
				NumberValue rnv = (NumberValue)rval;
				ret = new NumberValue( lnv.getNumber().modulo(rnv.getNumber()) );
			}
			break;
		
			/**
			 * MUST be HEX
			 */
		case OPERATOR_SHIFTL :
			{
				lval.verifyType(Value.VALUE_HEX);
				rval.verifyType(Value.VALUE_NUMBER);
				HexValue    lhv = (HexValue)lval;
				NumberValue rnv = (NumberValue)rval;
				ret = new HexValue( lhv.getMiniData().shiftl(rnv.getNumber().getAsInt()).to0xString() );
			}
			break;
		case OPERATOR_SHIFTR :
			{
				lval.verifyType(Value.VALUE_HEX);
				rval.verifyType(Value.VALUE_NUMBER);
				HexValue    lhv = (HexValue)lval;
				NumberValue rnv = (NumberValue)rval;
				ret = new HexValue( lhv.getMiniData().shiftr(rnv.getNumber().getAsInt()).to0xString() );
			}
			break;
			
			/**
			 * MUST be HEX
			 */
		case OPERATOR_AND :
			{
				Value.checkSameType(lval, rval,Value.VALUE_HEX);
				HexValue lhv  = (HexValue)lval;
				HexValue rhv  = (HexValue)rval;
				
				BigInteger lbig = lhv.getMiniData().getDataValue();
				BigInteger rbig = rhv.getMiniData().getDataValue();
				
				ret = new HexValue ( lbig.and(rbig).toString(16) );
			}
			break;
		case OPERATOR_OR :
			{
				Value.checkSameType(lval, rval,Value.VALUE_HEX);
				HexValue lhv  = (HexValue)lval;
				HexValue rhv  = (HexValue)rval;
				
				BigInteger lbig = lhv.getMiniData().getDataValue();
				BigInteger rbig = rhv.getMiniData().getDataValue();
				
				ret = new HexValue ( lbig.or(rbig).toString(16) );
			}
			break;
		case OPERATOR_XOR :
			{
				Value.checkSameType(lval, rval,Value.VALUE_HEX);
				HexValue lhv  = (HexValue)lval;
				HexValue rhv  = (HexValue)rval;
				
				BigInteger lbig = lhv.getMiniData().getDataValue();
				BigInteger rbig = rhv.getMiniData().getDataValue();
				
				ret = new HexValue ( lbig.xor(rbig).toString(16) );
			}
			break;
			
		default :
			throw new ExecutionException("UNKNOWN operator");		
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
