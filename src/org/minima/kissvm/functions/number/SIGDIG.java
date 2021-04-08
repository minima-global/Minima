package org.minima.kissvm.functions.number;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniNumber;

public class SIGDIG extends MinimaFunction {

	public SIGDIG() {
		super("SIGDIG");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		NumberValue significantdigits 	= zContract.getNumberParam(0, this);
		NumberValue number 				= zContract.getNumberParam(1, this);
		
		MiniNumber actnum = significantdigits.getNumber();
		if(!actnum.floor().isEqual(actnum)) {
			throw new ExecutionException("SIGDIG precision must be to a whole Number");
		}
		
		if(significantdigits.getNumber().isLess(MiniNumber.ZERO)) {
			throw new ExecutionException("SIGDIG precision must be a positive whole number : "+significantdigits);
		}
		
		return new NumberValue(number.getNumber().setSignificantDigits(significantdigits.getNumber().getAsInt()));
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SIGDIG();
	}
}