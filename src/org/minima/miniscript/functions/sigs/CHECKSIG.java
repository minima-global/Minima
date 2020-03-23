package org.minima.miniscript.functions.sigs;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.PubPrivKey;
import org.minima.objects.base.MiniHash;

/**
 * for now only retur  true..
 * 
 * @author spartacusrex
 *
 */
public class CHECKSIG extends MinimaFunction {

	public CHECKSIG() {
		super("CHECKSIG");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Get the Pbkey
		HEXValue pubkey = (HEXValue) getParameter(0).getValue(zContract);
		
		//get the data
		HEXValue data   = (HEXValue) getParameter(1).getValue(zContract);
		
		//Get the signature
		HEXValue sig    = (HEXValue) getParameter(2).getValue(zContract);
		
		//Check it..
		MiniHash pubk = new MiniHash(pubkey.getMiniData().getData());
		boolean ok = PubPrivKey.verify(pubk, new MiniHash(data.getRawData()), sig.getMiniData());
		
		// TODO Auto-generated method stub
		return new BooleanValue(ok);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CHECKSIG();
	}

}
