package org.minima.miniscript.functions.state;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.Value;

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
