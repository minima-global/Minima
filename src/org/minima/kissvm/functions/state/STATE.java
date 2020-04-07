package org.minima.kissvm.functions.state;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class STATE extends MinimaFunction {

	public STATE() {
		super("STATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Which Output - must be from 0-255
		int statenum = getParameter(0).getValue(zContract).getNumber().getAsInt();
				
		String stateval = zContract.getState(statenum).toString();
		stateval = Contract.cleanScript(stateval);
		
		//Work it out
		return Value.getValue(stateval);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new STATE();
	}
}
