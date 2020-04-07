package org.minima.kissvm.functions.state;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.Value;

public class PREVSTATE extends MinimaFunction {

	public PREVSTATE() {
		super("PREVSTATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Which Output - must be from 0-255
		int statenum = getParameter(0).getValue(zContract).getNumber().getAsInt();
				
		//Work it out
		return zContract.getPrevState( statenum );
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new PREVSTATE();
	}
}
