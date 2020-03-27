package org.minima.miniscript.expressions;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.values.Value;

public class GlobalExpression implements Expression {

	/**
	 * What type of Global id this
	 */
	String mGlobalType;

	/**
	 * Main Constructor
	 * 
	 * @param zType
	 */
	public GlobalExpression(String zType) {
		mGlobalType = zType;
	}
	
	@Override
	public Value getValue(Contract zContract) throws ExecutionException {
		return zContract.getGlobal(mGlobalType);
	}

	@Override
	public String toString() {
		return "global:"+mGlobalType;
	}
}
