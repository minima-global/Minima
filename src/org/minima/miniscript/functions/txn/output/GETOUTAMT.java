package org.minima.miniscript.functions.txn.output;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;

public class GETOUTAMT extends MinimaFunction {

	public GETOUTAMT() {
		super("GETOUTAMT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Which Output
		int output = getParameter(0).getValue(zContract).getNumber().getAsInt();
		
		//Get the Transaction
		Transaction trans = zContract.getTransaction();
		
		//Check output exists..
		ArrayList<Coin> outs = trans.getAllOutputs();
		if(outs.size()<=output) {
			throw new ExecutionException("Output number too high "+output+"/"+outs.size());
		}
		
		//Get it..
		Coin cc = outs.get(output);
		
		//Is it a Token..
		if(!cc.getTokenID().isExactlyEqual(Coin.MINIMA_TOKENID)) {
			//Get the Multiple..
			TokenProof td = zContract.getWitness().getTokenDetail(cc.getTokenID());
			return new NumberValue(cc.getAmount().mult(td.getScaleFactor()));
		}
		
		//Return the address	
		return new NumberValue(cc.getAmount());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new GETOUTAMT();
	}
}
