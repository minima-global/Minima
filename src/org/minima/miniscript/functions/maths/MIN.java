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
public class MIN extends MinimaFunction {

	public MIN() {
		super("MIN");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Run through the function parameters and pick the maximum numeric value..
		ArrayList<Expression> params = getAllParameters();
		
		boolean first 	= true;
		Value min 		= null;
		
		for(Expression exp : params) {
			//Get the Value
			Value chk = exp.getValue(zContract);
			
			if(first) {
				first 	= false;
				min 	= chk;
			}else {
				if(chk.getNumber().isLess(min.getNumber())) {
					min = chk;
				}
			}
		}
		
		return min;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new MIN();
	}

}
