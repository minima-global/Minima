/**
 * 
 */
package org.minima.miniscript.functions.base;

import java.nio.charset.Charset;
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
public class HEXCAT extends MinimaFunction{

	public HEXCAT() {
		super("HEXCAT");
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
			throw new ExecutionException("HEXCAT requires at least 1 parameter");
		}
		
		//Sum them
		int totlen  = 0;
		int counter = 0;
		for(Expression exp : params) {
			//Get the bytes
			parambytes[counter] = exp.getValue(zContract).getRawData();
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
