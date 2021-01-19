package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniNumber;

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
		Value val = getParameter(0).getValue(zContract);
		
		if(val.getValueType() == Value.VALUE_BOOLEAN) {
			if(val.isTrue()) {
				return new NumberValue(1);
			}else{
				return new NumberValue(0);
			}
		
		}else if(val.getValueType() == Value.VALUE_HEX) {
			HEXValue nv = (HEXValue)val;
			MiniNumber num = new MiniNumber(nv.getMiniData().getDataValue());
			return new NumberValue(num);
	
		}else if(val.getValueType() == Value.VALUE_SCRIPT) {
			ScriptValue nv = (ScriptValue)val;
			return new HEXValue(nv.getMiniData().to0xString());
		}
		
	
		// TODO Auto-generated method stub
		return val;
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new NUMBER();
	}
}
