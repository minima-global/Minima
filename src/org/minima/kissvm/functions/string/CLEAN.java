package org.minima.kissvm.functions.string;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

/**
 * CLEAN a script to it's minimal correct representation
 * 
 * @author spartacusrex
 */
public class CLEAN extends MinimaFunction {

	public CLEAN() {
		super("CLEAN");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get the STRING value
		StringValue preclean = zContract.getStringParam(0, this);
		
		//Now create a UTF8 String
		String newstr = Contract.cleanScript(preclean.toString());
		
		return new StringValue(newstr);	
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CLEAN();
	}
}
