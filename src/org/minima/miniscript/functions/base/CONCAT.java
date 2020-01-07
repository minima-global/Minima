/**
 * 
 */
package org.minima.miniscript.functions.base;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class CONCAT extends MinimaFunction{

	public CONCAT() {
		super("CONCAT");
	}

	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		int paramnum = params.size();
		byte[][] parambytes = new byte[paramnum][];
		
		if(paramnum < 1) {
			throw new ExecutionException("Invalid number of Parameters for CONCAT "+paramnum);
		}
		
		//The first parameter determines if this is a SCRIPT or HEXVALUE
		int type = params.get(0).getValue(zContract).getValueType();
		
		int totlen  = 0;
		int counter = 0;
		for(Expression exp : params) {
			//Get the bytes
			parambytes[counter] = exp.getValue(zContract).getRawData();
			totlen += parambytes[counter].length;
			counter++;
		}
		
		//The result is placed in here
		byte[] result = new byte[totlen];
		
		//And sum
		int pos=0;
		for(int i=0;i<counter;i++) {
			System.arraycopy(parambytes[i], 0, result, pos, parambytes[i].length);
			pos += parambytes[i].length;
		}
		
		if(type == HEXValue.VALUE_HEX) {
			return new HEXValue(result);	
		
		}else if(type == ScriptValue.VALUE_SCRIPT) {
			return new ScriptValue(new String(result));	
		
		}else {
			throw new ExecutionException("Invaid Value Type in CONCAT "+type+") "+params.get(0).toString());
		}
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CONCAT();
	}

}
