/**
 * 
 */
package org.minima.kissvm.functions.number;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

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
		checkMinParamNumber(requiredParams());
		
		//Run through the function parameters and pick the maximum numeric value..
		ArrayList<Expression> params = getAllParameters();
		
		boolean first 		= true;
		NumberValue min 	= null;
		
		for(Expression exp : params) {
			Value numval = exp.getValue(zContract);
			checkIsOfType(numval, Value.VALUE_NUMBER);
			
			//Get the Value
			NumberValue chk = (NumberValue)numval;
			
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
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new MIN();
	}

}
