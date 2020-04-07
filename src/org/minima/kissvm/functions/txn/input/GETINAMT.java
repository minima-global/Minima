package org.minima.kissvm.functions.txn.input;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.proofs.TokenProof;

public class GETINAMT extends MinimaFunction {

	public GETINAMT() {
		super("GETINAMT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Which Output - must be from 0-255
		int input = getParameter(0).getValue(zContract).getNumber().getAsInt();
		
		//Get the Transaction
		Transaction trans = zContract.getTransaction();
		
		//Check output exists..
		ArrayList<Coin> ins = trans.getAllInputs();
		if(ins.size()<=input) {
			throw new ExecutionException("Input number too high "+input+"/"+ins.size());
		}
		
		//Get it..
		Coin cc = ins.get(input);
		
		//Is it a Token..
		if(!cc.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
			//Get the Multiple..
			TokenProof td = zContract.getWitness().getTokenDetail(cc.getTokenID());
			return new NumberValue(cc.getAmount().mult(td.getScaleFactor()));
		}
		
		//Return the address	
		return new NumberValue(cc.getAmount());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new GETINAMT();
	}
}
