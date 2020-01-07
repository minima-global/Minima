package org.minima.miniscript.functions.sigs;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.Value;

public class SIGNEDBY extends MinimaFunction{

	public SIGNEDBY() {
		super("SIGNEDBY");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the Pub Key
		Value pubkey = getParameter(0).getValue(zContract);
		
		//Check it..
		boolean valid = zContract.checkSignature(pubkey);
		
		//return value
		return new BooleanValue(valid);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new SIGNEDBY();
	}
}
