package org.minima.kissvm.functions.string;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

public class STRING extends MinimaFunction {

	public STRING() {
		super("STRING");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
		Value val = getParameter(0).getValue(zContract);
		
		return new StringValue(val.toString());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new STRING();
	}
}
