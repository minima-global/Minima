/**
 * 
 */
package org.minima.kissvm.expressions;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class FunctionExpression implements Expression {
	
	MinimaFunction mFunction;
	
	/**
	 * Create the Function Expression 
	 */
	public FunctionExpression(MinimaFunction zFunction) {
		//Store for later
		mFunction = zFunction;
	}
	
	@Override
	public Value getValue(Contract zContract) throws ExecutionException {
		//This action counts as one instruction
		zContract.incrementInstructions();
				
		//Get the Value
		Value val = mFunction.runFunction(zContract);
		
		//And trace it..
		zContract.traceLog(toString()+" returns:"+val.toString());
		
		return val;
	}
	
	@Override
	public String toString() {
		return "function:"+mFunction.getName()+", params:"+mFunction.getAllParameters();
	}
}
