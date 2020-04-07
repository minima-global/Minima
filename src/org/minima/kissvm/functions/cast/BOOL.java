package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

public class BOOL extends MinimaFunction {

	public BOOL() {
		super("BOOL");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the Value..
		Value val = getParameter(0).getValue(zContract);
		
		// TODO Auto-generated method stub
		return new BooleanValue(val.isTrue());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new BOOL();
	}
}
