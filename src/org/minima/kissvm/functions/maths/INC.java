package org.minima.kissvm.functions.maths;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

public class INC extends MinimaFunction {

	public INC() {
		super("INC");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		NumberValue number = (NumberValue) getParameter(0).getValue(zContract);
		
		return new NumberValue(number.getNumber().increment());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new INC();
	}
}
