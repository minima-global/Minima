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

public class HEX extends MinimaFunction{

	public HEX() {
		super("HEX");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get the Value..
		Value val = getParameter(0).getValue(zContract);
		
		//What Type..
		MiniData ret = null;
		int type = val.getValueType();
		if(type == Value.VALUE_BOOLEAN) {
			BooleanValue cval = (BooleanValue)val;
			if(cval.isTrue()) {
				ret = new MiniData("0x01");
			}else {
				ret = new MiniData("0x00");
			}
		
		}else if(type == Value.VALUE_HEX) {
			HexValue cval = (HexValue)val;
			ret = cval.getMiniData();
		
		}else if(type == Value.VALUE_NUMBER) {
			NumberValue cval = (NumberValue)val;
			
			//Check no decimal places..
			MiniNumber num = cval.getNumber();
			if(!num.floor().isEqual(num) || num.isLess(MiniNumber.ZERO)) {
				throw new ExecutionException("Can ONLY convert positive whole NUMBERs to HEX : "+num);
			}
			
			ret = new MiniData(num.getAsBigInteger());
		
		}else if(type == Value.VALUE_SCRIPT) {
			StringValue cval = (StringValue)val;
			ret = new MiniData(cval.getBytes());
		
		}else {
			throw new ExecutionException("Invalid Type in HEX cast "+type);
		}
		
		return new HexValue(ret);
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new HEX();
	}
}
