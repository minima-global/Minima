package org.minima.kissvm.functions.txn.output;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class SUMOUTPUTS extends MinimaFunction {

	public SUMOUTPUTS() {
		super("SUMOUTPUTS");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Which Output
		MiniData tokenid = zContract.getHexParam(0, this).getMiniData();
		
		//Get all the inputs
		ArrayList<Coin> outs = zContract.getTransaction().getAllOutputs();
		
		//Get all the values of this tokenid
		MiniNumber total = MiniNumber.ZERO;
		for(Coin output : outs ) {
			
			//Make sure is the correct tokenid
			if(output.getTokenID().isEqual(tokenid)) {
				
				//Add to the total
				total = total.add(output.getAmount());
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
		return new SUMOUTPUTS();
	}
}
