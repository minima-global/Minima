package org.minima.kissvm.functions.base;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;

public class LEN extends MinimaFunction{

	public LEN() {
		super("LEN");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Check Params
		checkExactParamNumber(1);
		
		//The Data
		HEXValue hex = zContract.getHEXParam(0, this);
		int len      = hex.getRawData().length;
		
		return new NumberValue(len);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new LEN();
	}
}
