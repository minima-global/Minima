package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniNumber;

public class BOOL extends MinimaFunction {

	public BOOL() {
		super("BOOL");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
		//Get the Value..
		Value val = getParameter(0).getValue(zContract);
		
		//What Type..
		boolean ret = false;
		int type = val.getValueType();
		if(type == Value.VALUE_BOOLEAN) {
			BooleanValue cval = (BooleanValue)val;
			ret = cval.isTrue();
		
		}else if(type == Value.VALUE_HEX) {
			HEXValue cval = (HEXValue)val;
			
			//Convert to a mininumber - this ensures is not TOO big a data structure
			MiniNumber num = new MiniNumber(cval.getMiniData().getDataValue());
			
			//0 is FALSE
			ret = !num.isEqual(MiniNumber.ZERO);
		
		}else if(type == Value.VALUE_NUMBER) {
			NumberValue cval = (NumberValue)val;
			
			//0 is FALSE
			ret = !cval.getNumber().isEqual(MiniNumber.ZERO);
		
		}else if(type == Value.VALUE_SCRIPT) {
			ScriptValue cval = (ScriptValue)val;
			
			//check for FALSE - everything else is TRUE
			ret = !cval.toString().equals("FALSE");
		
		}else {
			throw new ExecutionException("Invalid Type in BOOL cast "+type);
		}
		
		return new BooleanValue(ret);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new BOOL();
	}
}
