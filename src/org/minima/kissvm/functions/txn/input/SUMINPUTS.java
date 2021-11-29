package org.minima.kissvm.functions.txn.input;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class SUMINPUTS extends MinimaFunction {

	public SUMINPUTS() {
		super("SUMINPUTS");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Which Output
		MiniData tokenid = zContract.getHexParam(0, this).getMiniData();
		
		//Get all the inputs
		ArrayList<Coin> ins = zContract.getTransaction().getAllInputs();
		
		//Get all the values of this tokenid
		MiniNumber total = MiniNumber.ZERO;
		for(Coin input : ins ) {
			
			//Make sure is the correct tokenid
			if(input.getTokenID().isEqual(tokenid)) {
				
				//Add to the total
				total = total.add(input.getAmount());
			}	
		}
		
		//Return the Amount
		return new NumberValue(total);
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SUMINPUTS();
	}
}
