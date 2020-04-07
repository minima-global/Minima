package org.minima.kissvm.functions.base;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

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
