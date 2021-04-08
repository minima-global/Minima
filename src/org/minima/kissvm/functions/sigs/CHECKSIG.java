package org.minima.kissvm.functions.sigs;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.MultiKey;

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
		checkExactParamNumber(3);
		
		//Get the Pbkey
		HexValue pubkey = zContract.getHexParam(0, this);
		
		//get the data
		HexValue data   = zContract.getHexParam(1, this);
		
		//Get the signature
		HexValue sig    = zContract.getHexParam(2, this);
		
		//Check it..
		MiniData pubk = new MiniData(pubkey.getMiniData().getData());
		
		//Simple checks..
		if(pubk.getLength() == 0 || sig.getMiniData().getLength()==0) {
			throw new ExecutionException("Invalid ZERO length param,s for CHECKSIG");
		}
		
		//Create a MultiKey to check the signature
		MultiKey checker = new MultiKey();
		checker.setPublicKey(pubk);
		
		//Check it..
		boolean ok = checker.verify(new MiniData(data.getRawData()), sig.getMiniData());
		
		return new BooleanValue(ok);
	}
	
	@Override
	public int requiredParams() {
		return 3;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CHECKSIG();
	}

}
