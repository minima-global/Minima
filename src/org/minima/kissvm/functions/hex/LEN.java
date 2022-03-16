package org.minima.kissvm.functions.hex;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

public class LEN extends MinimaFunction{

	public LEN() {
		super("LEN");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//The Data
		Value val 	= getParameter(0).getValue(zContract);
		
		if(val.getValueType() == Value.VALUE_HEX) {
			
			HexValue hv = (HexValue)val;
			int len     = hv.getRawData().length;
			
			return new NumberValue(len);
		
		}else if(val.getValueType() == Value.VALUE_SCRIPT) {
			
			StringValue sv 	= (StringValue)val;
			int len     	= sv.toString().length();
			
			return new NumberValue(len);
		}
		
		throw new ExecutionException("LEN requires HEX or STRING param @ "+val.toString());
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new LEN();
	}
}
