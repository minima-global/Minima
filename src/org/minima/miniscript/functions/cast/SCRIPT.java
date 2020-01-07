package org.minima.miniscript.functions.cast;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;

public class SCRIPT extends MinimaFunction {

	public SCRIPT() {
		super("SCRIPT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		Value val = getParameter(0).getValue(zContract);
		int type  = val.getValueType();
		
		if(type == NumberValue.VALUE_NUMBER) {
			return new ScriptValue(""+val.getNumber().toString());
		}else if(type == HEXValue.VALUE_HEX) {
			return new ScriptValue(""+val.getMiniData().toString());
		}else if(type == BooleanValue.VALUE_BOOLEAN) {
			return new ScriptValue(""+val.toString());
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
