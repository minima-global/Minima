package org.minima.kissvm.functions.maths;

import java.math.BigDecimal;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

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