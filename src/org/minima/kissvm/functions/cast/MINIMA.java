package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniNumber;

/**
 * Convert a NUMBERVALUE to a valid Minima value
 * 
 * @author spartacusrex
 *
 */
public class MINIMA extends MinimaFunction{

	public MINIMA() {
		super("MINIMA");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
		//Get the Value..
		NumberValue num = zContract.getNumberParam(0, this);
		
		//Convert to a valid Minima Value
		MiniNumber minima = num.getNumber().getAsMinimaValue();
		
		return new NumberValue(minima);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new MINIMA();
	}
}
