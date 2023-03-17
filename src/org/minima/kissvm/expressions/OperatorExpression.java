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
	public static final int OPERATOR_NOT   		= 11;
	
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
				//Check same on both sides
				Value.checkSameType(lval, rval);
				
				if(lval.getValueType() == Value.VALUE_NUMBER) {
					NumberValue lnv = (NumberValue)lval;
					NumberValue rnv = (NumberValue)rval;
					return new NumberValue(lnv.getNumber().add(rnv.getNumber()));
					
				}else if(lval.getValueType() == Value.VALUE_SCRIPT) {
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
				
				//Can only SHIFT max amount..
				if(rnv.getNumber().abs().isMore(Contract.MAX_BITSHIFT)) {
					throw new ExecutionException("Can only SHIFTLEFT "+Contract.MAX_BITSHIFT+" bits MAX "+rnv.getNumber().toString());
				}
				
				ret = new HexValue( lhv.getMiniData().shiftl(rnv.getNumber().getAsInt()) );
			}
			break;
		case OPERATOR_SHIFTR :
			{
				lval.verifyType(Value.VALUE_HEX);
				rval.verifyType(Value.VALUE_NUMBER);
				HexValue    lhv = (HexValue)lval;
				NumberValue rnv = (NumberValue)rval;
				
				//Can only SHIFT max amount..
				if(rnv.getNumber().abs().isMore(Contract.MAX_BITSHIFT)) {
					throw new ExecutionException("Can only SHIFTRIGHT "+Contract.MAX_BITSHIFT+" bits MAX "+rnv.getNumber().toString());
				}
				
				ret = new HexValue( lhv.getMiniData().shiftr(rnv.getNumber().getAsInt()) );
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
				
//				BigInteger lbig = lhv.getMiniData().getDataValue();
//				BigInteger rbig = rhv.getMiniData().getDataValue();
//				ret = new HexValue ( lbig.or(rbig).toString(16) );
				
				MiniData result = orFastHEX(lhv.getMiniData(), rhv.getMiniData(),0);
				ret = new HexValue ( result );
			}
			break;
		case OPERATOR_XOR :
			{
				Value.checkSameType(lval, rval,Value.VALUE_HEX);
				HexValue lhv  = (HexValue)lval;
				HexValue rhv  = (HexValue)rval;
				
//				BigInteger lbig = lhv.getMiniData().getDataValue();
//				BigInteger rbig = rhv.getMiniData().getDataValue();
//				ret = new HexValue ( lbig.xor(rbig).toString(16) );
				
				MiniData result = orFastHEX(lhv.getMiniData(), rhv.getMiniData(),1);
				ret = new HexValue ( result );
			}
			break;
			
		case OPERATOR_NOT :
		{
			lval.verifyType(Value.VALUE_HEX);
			HexValue lhv  = (HexValue)lval;
			
			//NOT the data
			MiniData result = notFastHEX(lhv.getMiniData());
			ret = new HexValue ( result );
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
		case OPERATOR_NOT :
			return " ~ ( "+mLeft+" )";
		}
		
		return "( "+mLeft + " "+ret+" " + mRight+" )";
	}
	
	public MiniData andFastHEX(MiniData zHex1, MiniData zHex2) {
		
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
	
	public MiniData orFastHEX(MiniData zHex1, MiniData zHex2, int zType) {
		
		//Get the bytes
		byte[] bytesh1 = zHex1.getBytes();
		byte[] bytesh2 = zHex2.getBytes();
		
		//Get the lengths
		int len1 = bytesh1.length;
		int len2 = bytesh2.length;
		
		//First find the smallest
		boolean hex1longer = true;
		int maxlen = len1;
		if(len2 > maxlen) {
			maxlen = len2;
			hex1longer = false;
		}
		
		//Create the working sets
		byte[] pbytes1 	= new byte[maxlen];
		byte[] pbytes2 	= new byte[maxlen];
		
		//The result..
		int counter=0;
		byte[] result 	= new byte[maxlen];
		
		//Now copy the 2 datasets into correctly sized data structures
		if(hex1longer) {
			
			//Copy data
			System.arraycopy(bytesh1, 0, pbytes1, 0, maxlen);
			System.arraycopy(bytesh2, 0, pbytes2, maxlen - len2, len2);
			
		}else {
			
			//Copy data
			System.arraycopy(bytesh1, 0, pbytes1, maxlen - len1, len1);
			System.arraycopy(bytesh2, 0, pbytes2, 0, maxlen);
		}
		
		//Now AND everything
		boolean nonzerofound = false;
		
		//Is it OR or XOR
		if(zType==0) {
			for(int i=0;i<maxlen;i++) {
				
				//Do the OR
				byte bres = (byte) (pbytes1[i] | pbytes2[i]);
				
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
		}else {
			for(int i=0;i<maxlen;i++) {
				
				//Do the XOR
				byte bres = (byte) (pbytes1[i] ^ pbytes2[i]);
				
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
	
	public MiniData notFastHEX(MiniData zHex1) {
		//Get the bytes
		byte[] bytesh1 = zHex1.getBytes();
		
		//Get the lengths
		int len 		= bytesh1.length;
		byte[] result 	= new byte[len];
		
		//Now NOT everything
		boolean nonzerofound = false;
		int counter 		 = 0;
		
		for(int i=0;i<len;i++) {
			
			//Do the NOT
			byte bres = (byte) (~bytesh1[i]);
			
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
