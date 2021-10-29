package org.minima.kissvm.expressions;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.Value;

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
