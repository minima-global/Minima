package org.minima.kissvm.functions.state;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.Value;

public class STATE extends MinimaFunction {

	public STATE() {
		super("STATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
		//Which Output
		int statenum = zContract.getNumberParam(0, this).getNumber().getAsInt();

		//Work it out
		return zContract.getState(statenum);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new STATE();
	}
}
