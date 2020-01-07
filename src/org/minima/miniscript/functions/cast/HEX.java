package org.minima.miniscript.functions.cast;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;

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
