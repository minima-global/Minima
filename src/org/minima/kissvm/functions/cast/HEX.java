package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class HEX extends MinimaFunction{

	public HEX() {
		super("HEX");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
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
			HEXValue cval = (HEXValue)val;
			ret = cval.getMiniData();
		
		}else if(type == Value.VALUE_NUMBER) {
			NumberValue cval = (NumberValue)val;
			
			//Check no decimal places..
			MiniNumber num = cval.getNumber();
			if(!num.floor().isEqual(num) || !num.abs().isEqual(num)) {
				throw new ExecutionException("Cannot ONLY convert positive whole NUMBERs to HEX : "+num);
			}
			
			ret = new MiniData(num.getAsBigInteger());
		
		}else if(type == Value.VALUE_SCRIPT) {
			ScriptValue cval = (ScriptValue)val;
			ret = new MiniData(cval.toString().getBytes());
		
		}else {
			throw new ExecutionException("Invalid Type in HEX cast "+type);
		}
		
		return new HEXValue(ret);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new HEX();
	}
}
