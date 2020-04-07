package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

public class SCRIPT extends MinimaFunction {

	public SCRIPT() {
		super("SCRIPT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		Value val = getParameter(0).getValue(zContract);
		int type  = val.getValueType();
		
		if(type == NumberValue.VALUE_NUMBER) {
			return new ScriptValue(val.getNumber().toString());
		}else if(type == HEXValue.VALUE_HEX) {
			return new ScriptValue(val.getMiniData().to0xString());
		}else if(type == BooleanValue.VALUE_BOOLEAN) {
			return new ScriptValue(val.toString());
		}else if(type == ScriptValue.VALUE_SCRIPT) {
			return val;
		}
		
		throw new ExecutionException("Incorrect Vaue Type! "+type);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new SCRIPT();
	}
}
