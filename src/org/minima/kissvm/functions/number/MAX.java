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
public class MAX extends MinimaFunction {

	public MAX() {
		super("MAX");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkMinParamNumber(requiredParams());
		checkAllParamsType(Value.VALUE_NUMBER, zContract);
		
		//Run through the function parameters and pick the maximum numeric value..
		ArrayList<Expression> params = getAllParameters();
		
		boolean first 		= true;
		NumberValue max 	= null;
		
		for(Expression exp : params) {
			//Get the Value
			NumberValue chk = (NumberValue)exp.getValue(zContract);
			
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
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new MAX();
	}
}
