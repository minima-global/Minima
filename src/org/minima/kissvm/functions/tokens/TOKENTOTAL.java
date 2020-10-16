package org.minima.kissvm.functions.tokens;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.proofs.TokenProof;

public class TOKENTOTAL extends MinimaFunction {

	public TOKENTOTAL() {
		super("TOKENTOTAL");
	}	
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the Token ID..
		HEXValue tokenid  = (HEXValue) getParameter(0).getValue(zContract);
		
		//Get that tokens details..
		TokenProof td = zContract.getWitness().getTokenDetail(tokenid.getMiniData());
		if(td == null) {
			throw new ExecutionException("No Token found for ID "+tokenid.toString());
		}
		
		// Return reversed value
		return new NumberValue(td.getTotalTokens());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new TOKENTOTAL();
	}
}
