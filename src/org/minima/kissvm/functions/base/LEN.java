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
		//The Data
		Value vv = getParameter(0).getValue(zContract);
		if(vv.getValueType() != Value.VALUE_HEX && vv.getValueType() == Value.VALUE_SCRIPT) {
			throw new ExecutionException("LEN requires HEXValue or ScriptValue");
		}
		HEXValue hex = (HEXValue)vv;
		
		//The Length
		int len = hex.getRawData().length;
		
		return new NumberValue(len);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new LEN();
	}
}
