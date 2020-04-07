package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

/**
 * Convert a HEXValue to a NUMBERVALUE
 * 
 * @author spartacusrex
 *
 */
public class NUMBER extends MinimaFunction{

	public NUMBER() {
		super("NUMBER");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the Value..
		Value hex = getParameter(0).getValue(zContract);
		
		//Create a Number Value
		NumberValue num = new NumberValue(hex.getNumber());
		
		// TODO Auto-generated method stub
		return num;
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new NUMBER();
	}
}
