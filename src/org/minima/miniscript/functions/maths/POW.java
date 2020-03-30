package org.minima.miniscript.functions.maths;

import java.math.BigDecimal;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

public class POW extends MinimaFunction {

	public POW() {
		super("POW");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		NumberValue exp = (NumberValue) getParameter(0).getValue(zContract);
		NumberValue number = (NumberValue) getParameter(1).getValue(zContract);
		
		//Only works for WHOLE numbers..
		return new NumberValue(number.getNumber().pow(exp.getNumber().getAsInt()));
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new POW();
	}
}