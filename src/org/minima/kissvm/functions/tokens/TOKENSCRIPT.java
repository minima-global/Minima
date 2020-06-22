package org.minima.kissvm.functions.tokens;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.proofs.TokenProof;

public class TOKENSCRIPT extends MinimaFunction {

	public TOKENSCRIPT() {
		super("TOKENSCRIPT");
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
		return new ScriptValue(td.getTokenScript().toString());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new TOKENSCRIPT();
	}
}
