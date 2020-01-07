/**
 * 
 */
package org.minima.miniscript.expressions;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.Value;

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
		zContract.countInstructions();
				
		return mFunction.runFunction(zContract);
	}
	
	@Override
	public String toString() {
		return "function:"+mFunction.getName()+", params:"+mFunction.getAllParameters();
	}
}
