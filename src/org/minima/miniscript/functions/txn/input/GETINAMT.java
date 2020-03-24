package org.minima.miniscript.functions.txn.input;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;
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
		return new GETINAMT();
	}
}
