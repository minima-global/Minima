package org.minima.kissvm.functions.state;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class SAMESTATE extends MinimaFunction {

	public SAMESTATE() {
		super("SAMESTATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Is this a one off or a sequence..
		int start = getParameter(0).getValue(zContract).getNumber().getAsInt();
		int end   = start;
		
		if(getParameterNum() == 2) {
			end = getParameter(1).getValue(zContract).getNumber().getAsInt();
		}
		
		//Now check the old state and the current state are the same
		for(int i=start;i<=end;i++) {
			//Get the state variable..
			//Get the old state..
			String olds = zContract.getPrevState(i).toString();
			String news = zContract.getState(i).toString();
			
			//check the same
			if(!olds.equals(news)) {
				zContract.traceLog("SAMESTATE FAIL ["+i+"] PREV:"+olds+" / CURRENT:"+news);
				return BooleanValue.FALSE;
			}
		}
		
		return BooleanValue.TRUE;
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new SAMESTATE();
	}
}
