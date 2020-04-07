package org.minima.kissvm.functions.sigs;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.cast.HEX;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

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
