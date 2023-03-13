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
					
					StringValue res = lnv.add(rnv);
					
					return res;
				
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
				
				//Can only SHIFT max amount..
				if(rnv.getNumber().isMore(MiniNumber.TWOFIVESIX)) {
					throw new ExecutionException("Can only SHIFTLEFT 256 bits MAX");
				}
				
				ret = new HexValue( lhv.getMiniData().shiftl(rnv.getNumber().getAsInt()).to0xString() );
			}
			break;
		case OPERATOR_SHIFTR :
			{
				lval.verifyType(Value.VALUE_HEX);
				rval.verifyType(Value.VALUE_NUMBER);
				HexValue    lhv = (HexValue)lval;
				NumberValue rnv = (NumberValue)rval;
				
				//Can only SHIFT max amount..
				if(rnv.getNumber().isMore(MiniNumber.TWOFIVESIX)) {
					throw new ExecutionException("Can only SHIFTRIGHT 256 bits MAX");
				}
				
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
				
//				BigInteger lbig = lhv.getMiniData().getDataValue();
//				BigInteger rbig = rhv.getMiniData().getDataValue();
//				ret = new HexValue ( lbig.and(rbig).toString(16) );
				
				MiniData result = andFastHEX(lhv.getMiniData(), rhv.getMiniData());
				ret = new HexValue ( result );
				
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
	
	public static void main(String[] zArgs) {
		
		long timenow 	= System.currentTimeMillis(); 
		
		//Create 2 MiniData structure..
		MiniData s1 = new MiniData("0x00");
		MiniData s2 = new MiniData("0x01");
		
//		int tot = 65536;
//		MiniData s1 = MiniData.getRandomData(tot);
//		MiniData s2 = MiniData.getRandomData(tot);
		
		//Do it..
		MiniData minires = andFastHEX(s1, s2);
		
		long timediff = System.currentTimeMillis() - timenow; 
		
		
		System.out.println("S1   : "+s1.to0xString());
		System.out.println("S2   : "+s2.to0xString());
		System.out.println("RES  : "+minires.to0xString());
		System.out.println("Time : "+timediff+"ms");
		
	}
	
	public static MiniData andFastHEX(MiniData zHex1, MiniData zHex2) {
		
		//Get the bytes
		byte[] bytesh1 = zHex1.getBytes();
		byte[] bytesh2 = zHex2.getBytes();
		
		//Get the lengths
		int len1 = bytesh1.length;
		int len2 = bytesh2.length;
		
		//First find the smallest
		boolean hex1shorter = true;
		int minlen = len1;
		if(len2 < minlen) {
			minlen = len2;
			hex1shorter = false;
		}
		
		//Create the working sets
		byte[] pbytes1 	= new byte[minlen];
		byte[] pbytes2 	= new byte[minlen];
		
		//The result..
		int counter=0;
		byte[] result 	= new byte[minlen];
		
		//Now copy the 2 datasets into correctly sized data structures
		if(hex1shorter) {
			
			//Copy data
			System.arraycopy(bytesh1, 0, pbytes1, 0, minlen);
			System.arraycopy(bytesh2, len2 - minlen, pbytes2, 0, minlen);
			
		}else {
			
			//Copy data
			System.arraycopy(bytesh1, len1 - minlen, pbytes1, 0, minlen);
			System.arraycopy(bytesh2, 0, pbytes2, 0, minlen);
		}
		
		//Now AND everything
		boolean nonzerofound = false;
		for(int i=0;i<minlen;i++) {
			
			//Do the AND
			byte bres = (byte) (pbytes1[i] & pbytes2[i]);
			
			//Skip leading ZEROs
			if(nonzerofound) {
				result[counter++] = bres;
			}else {
				if(bres != 0) {
					nonzerofound 		= true;
					result[counter++] 	= bres;
				}
			}
		}

		//If NONE added return 0
		if(counter==0) {
			return new MiniData("0x00");
		}
		
		//Now copy the data..
		byte[] finalresult = new byte[counter];
		System.arraycopy(result, 0, finalresult, 0, counter);
		
		return new MiniData(finalresult);
	}
}
