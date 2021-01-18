/**
 * 
 */
package org.minima.kissvm.functions.base;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.Value;

/**
 * Concatenate HEX values
 * 
 * @author Spartacus Rex
 *
 */
public class HEXCAT extends MinimaFunction{

	public HEXCAT() {
		super("HEXCAT");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		int paramnum = params.size();
		byte[][] parambytes = new byte[paramnum][];
		
		if(paramnum < 1) {
			throw new ExecutionException("HEXCAT requires at least 1 parameter");
		}
		
		//Sum them
		int totlen  = 0;
		int counter = 0;
		for(Expression exp : params) {
			Value vv = exp.getValue(zContract);
			if(vv.getValueType() != Value.VALUE_HEX) {
				throw new ExecutionException("All parameters to HEXCAT MUST be HEXValue");
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
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new HEXCAT();
	}

}
