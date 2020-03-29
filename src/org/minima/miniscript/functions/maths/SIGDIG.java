package org.minima.miniscript.functions.maths;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

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