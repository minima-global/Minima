package org.minima.miniscript.functions.cast;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.Value;

public class BOOL extends MinimaFunction {

	public BOOL() {
		super("BOOL");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the Value..
		Value val = getParameter(0).getValue(zContract);
		
		// TODO Auto-generated method stub
		return new BooleanValue(val.isTrue());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new BOOL();
	}
}
