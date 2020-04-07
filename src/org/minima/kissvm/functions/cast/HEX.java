package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.Value;

public class HEX extends MinimaFunction{

	public HEX() {
		super("HEX");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the Value..
		Value val = getParameter(0).getValue(zContract);
		
		//Create a Number Value
		HEXValue hex = new HEXValue(val.getMiniData());
		
		// TODO Auto-generated method stub
		return hex;
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new HEX();
	}
}
