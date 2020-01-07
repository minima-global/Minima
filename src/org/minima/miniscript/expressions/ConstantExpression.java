/**
 * 
 */
package org.minima.miniscript.expressions;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class ConstantExpression implements Expression{

	private Value mValue;
	
	public ConstantExpression(Value zValue) {
		mValue = zValue;
	}
	
	@Override
	public Value getValue(Contract zContract) throws ExecutionException {
		return mValue;
	}
	
	@Override
	public String toString() {
		return mValue.toString();
	}
}
