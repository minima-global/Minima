package org.minima.miniscript.functions.maths;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

public class DEC extends MinimaFunction {

	public DEC() {
		super("DEC");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		NumberValue number = (NumberValue) getParameter(0).getValue(zContract);
		
		return new NumberValue(number.getNumber().decrement());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new DEC();
	}
}
