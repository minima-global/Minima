package org.minima.miniscript.functions.cast;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

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
