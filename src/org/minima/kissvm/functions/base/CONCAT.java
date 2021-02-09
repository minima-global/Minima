package org.minima.kissvm.functions.base;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

public class CONCAT extends MinimaFunction{

	public CONCAT() {
		super("CONCAT");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Check parameters..
		checkMinParamNumber(2);
		
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		
		//Check first param..
		Value valcheck  = params.get(0).getValue(zContract);
		boolean isHex = false;
		int ptype = valcheck.getValueType();
		if(ptype == Value.VALUE_HEX) {
			isHex = true;
		}else if(ptype == Value.VALUE_SCRIPT) {
			isHex = false;
		}else {
			throw new ExecutionException("CONCAT parameters must be HEX or SCRIPT");
		}
		
		//Check all the same type
		checkAllParamsType(ptype, zContract);
		
		//Is it HEX or SCRIPT
		if(isHex) {
			//Sum them
			byte[][] parambytes = new byte[getAllParameters().size()][];
			int totlen  = 0;
			int counter = 0;
			for(Expression exp : params) {
				Value vv = exp.getValue(zContract);
				
				//This is a HEXValue
				HEXValue hex = (HEXValue)vv;
				
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
			
			return new HEXValue(result);
		
		}else {
			//Sum them
			String fullstring = "";
			for(Expression exp : params) {
				Value vv = exp.getValue(zContract);
				
				//This is a ScriptValue
				ScriptValue scr = (ScriptValue)vv;
				
				//Add it..
				fullstring += scr.toString()+" ";
			}
				
			return new ScriptValue(fullstring.trim());
		}
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CONCAT();
	}
}
