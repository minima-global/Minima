package org.minima.kissvm.functions.tokens;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.proofs.TokenProof;

public class TOKENSCALE extends MinimaFunction {

	public TOKENSCALE() {
		super("TOKENSCALE");
	}	
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
		//get the Token ID..
		HexValue tokenid  = zContract.getHEXParam(0, this);
		
		//Get that tokens details..
		TokenProof td = zContract.getWitness().getTokenDetail(tokenid.getMiniData());
		if(td == null) {
			throw new ExecutionException("No Token found for ID "+tokenid.toString());
		}
		
		return new NumberValue(td.getScaleFactor());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new TOKENSCALE();
	}
}
