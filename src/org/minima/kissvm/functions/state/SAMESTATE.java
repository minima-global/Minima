package org.minima.kissvm.functions.state;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

/**
 * Check the states (inclusive) are the same from previous and current
 * 
 * States MUST exist in PREV to be checked
 * 
 * @author spartacusrex
 *
 */
public class SAMESTATE extends MinimaFunction {

	public SAMESTATE() {
		super("SAMESTATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Is this a one off or a sequence..
		int start = zContract.getNumberParam(0, this).getNumber().getAsInt();
		int end   = zContract.getNumberParam(1, this).getNumber().getAsInt();
		
		//Simple checks..
		if(start<0 || end<start) {
			throw new ExecutionException("Invalid range check for SAMESTATE "+start+" "+end);
		}
		
		//Now check the old state and the current state are the same
		for(int i=start;i<=end;i++) {
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
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SAMESTATE();
	}
}
