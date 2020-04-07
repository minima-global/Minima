package org.minima.kissvm.functions.maths;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

public class SIGDIG extends MinimaFunction {

	public SIGDIG() {
		super("SIGDIG");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		NumberValue significantdigits = (NumberValue) getParameter(0).getValue(zContract);
		NumberValue number = (NumberValue) getParameter(1).getValue(zContract);
		
		return new NumberValue(number.getNumber().setSignificantDigits(significantdigits.getNumber().getAsInt()));
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SIGDIG();
	}
}