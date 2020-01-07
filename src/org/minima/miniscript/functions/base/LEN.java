package org.minima.miniscript.functions.base;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

public class LEN extends MinimaFunction{

	public LEN() {
		super("LEN");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//The Data
		Value hex = getParameter(0).getValue(zContract);
		
		//The Length
		int len = hex.getRawData().length;
		
		return new NumberValue(len);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new LEN();
	}
}
