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
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		
		//Data structures required
		int paramnum = params.size();
		if(paramnum < 2) {
			throw new ExecutionException("CONCAT requires at least 2 parameters");
		}
		
		//Check first param..
		Value valcheck  = params.get(0).getValue(zContract);
		boolean isHex = false;
		if(valcheck.getValueType() == Value.VALUE_HEX) {
			isHex = true;
		}else if(valcheck.getValueType() == Value.VALUE_SCRIPT) {
			isHex = false;
		}else {
			throw new ExecutionException("CONCAT parameters must be HEX or SCRIPT");
		}
		
		if(isHex) {
			//Sum them
			byte[][] parambytes = new byte[paramnum][];
			int totlen  = 0;
			int counter = 0;
			for(Expression exp : params) {
				Value vv = exp.getValue(zContract);
				if(vv.getValueType() != Value.VALUE_HEX) {
					throw new ExecutionException("All parameters to CONCAT MUST be of same type");
				}
				
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
				if(vv.getValueType() != Value.VALUE_SCRIPT) {
					throw new ExecutionException("All parameters to CONCAT MUST be of same type");
				}
				
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
