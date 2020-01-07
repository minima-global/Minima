/**
 * 
 */
package org.minima.miniscript.functions.maths;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class MAX extends MinimaFunction {

	public MAX() {
		super("MAX");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Run through the function parameters and pick the maximum numeric value..
		ArrayList<Expression> params = getAllParameters();
		
		boolean first 	= true;
		Value max 		= null;
		
		for(Expression exp : params) {
			//Get the Value
			Value chk = exp.getValue(zContract);
			
			if(first) {
				first 	= false;
				max 	= chk;
			}else {
				if(chk.getNumber().isMore(max.getNumber())) {
					max = chk;
				}
			}
		}
		
		return max;
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new MAX();
	}
}
