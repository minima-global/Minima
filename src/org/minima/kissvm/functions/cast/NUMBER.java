package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;
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
		checkExactParamNumber(requiredParams());
		
		//Get the Value..
		Value val = getParameter(0).getValue(zContract);
		
		int type = val.getValueType();
		if(type == Value.VALUE_BOOLEAN) {
			BooleanValue cval = (BooleanValue)val;
			if(cval.isTrue()) {
				return new NumberValue(1);
			}else{
				return new NumberValue(0);
			}
		
		}else if(type == Value.VALUE_HEX) {
			HexValue cval = (HexValue)val;
			MiniData md1 = cval.getMiniData();
			MiniNumber num = new MiniNumber(md1.getDataValue());
			return new NumberValue(num);
	
		}else if(type == Value.VALUE_SCRIPT) {
			StringValue cval = (StringValue)val;
			return new NumberValue(cval.toString());
		
		}else if(type == Value.VALUE_NUMBER) {
			NumberValue cval = (NumberValue)val;
			return new NumberValue(cval.getNumber());
		}
	
		throw new ExecutionException("Invalid Type in NUMBER cast "+type);
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new NUMBER();
	}
}
