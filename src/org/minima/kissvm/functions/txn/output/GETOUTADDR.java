package org.minima.kissvm.functions.txn.output;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.cast.HEX;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;

public class GETOUTADDR extends MinimaFunction {

	public GETOUTADDR() {
		super("GETOUTADDR");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Which Output - must be from 0-255
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
		
		//Return the address	
		return new HEXValue(cc.getAddress());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new GETOUTADDR();
	}
}
