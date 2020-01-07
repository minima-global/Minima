package org.minima.miniscript.functions.txn.output;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;

public class GETOUTTOK extends MinimaFunction {

	public GETOUTTOK() {
		super("GETOUTTOK");
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
		
		//Return the address	
		return new HEXValue(cc.getTokenID());
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new GETOUTTOK();
	}
}
