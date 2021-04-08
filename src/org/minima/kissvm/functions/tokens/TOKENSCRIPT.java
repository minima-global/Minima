package org.minima.kissvm.functions.tokens;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.proofs.TokenProof;

public class TOKENSCRIPT extends MinimaFunction {

	public TOKENSCRIPT() {
		super("TOKENSCRIPT");
	}	
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(1);
		
		//get the Token ID..
		HexValue tokenid  = zContract.getHexParam(0, this);
		
		//Get that tokens details..
		TokenProof td = zContract.getWitness().getTokenDetail(tokenid.getMiniData());
		if(td == null) {
			throw new ExecutionException("No Token found for ID "+tokenid.toString());
		}
		
		// Return reversed value
		return new StringValue(td.getTokenScript().toString());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new TOKENSCRIPT();
	}
}
