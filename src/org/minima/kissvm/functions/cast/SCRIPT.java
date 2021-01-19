package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

public class SCRIPT extends MinimaFunction {

	public SCRIPT() {
		super("SCRIPT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		Value val = getParameter(0).getValue(zContract);
		
		//HEX value gets converted..
		if(val.getValueType() == Value.VALUE_HEX) {
			HEXValue hex = (HEXValue)val;
			return new ScriptValue(hex.getRawData());
		}
		
		return new ScriptValue(val.toString());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new SCRIPT();
	}
}
