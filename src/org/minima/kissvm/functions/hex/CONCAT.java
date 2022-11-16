package org.minima.kissvm.functions.hex;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;

public class CONCAT extends MinimaFunction{

	public CONCAT() {
		super("CONCAT");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Check parameters..
		checkMinParamNumber(requiredParams());
		
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		
		//Check all the same type
		checkAllParamsType(Value.VALUE_HEX, zContract);
		
		//Sum them
		byte[][] parambytes = new byte[getAllParameters().size()][];
		int totlen  = 0;
		int counter = 0;
		for(Expression exp : params) {
			Value vv = exp.getValue(zContract);
			
			//This is a HEXValue
			HexValue hex = (HexValue)vv;
			
			//Get the bytes
			parambytes[counter] = hex.getRawData();
			totlen += parambytes[counter].length;
			counter++;
		}
		
		//The result is placed in here
		byte[] result     = new byte[totlen];
		//And sum
		int pos=0;
		for(int i=0;i<counter;i++) {
			//Is it RAW data
			System.arraycopy(parambytes[i], 0, result, pos, parambytes[i].length);
			pos += parambytes[i].length;
		}
		
		return new HexValue(result);
	}
	
	@Override
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CONCAT();
	}
}
