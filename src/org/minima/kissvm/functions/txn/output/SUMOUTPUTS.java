package org.minima.kissvm.functions.txn.output;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class SUMOUTPUTS extends MinimaFunction {

	public SUMOUTPUTS() {
		super("SUMOUTPUTS");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Which Token
		MiniData tokenid = zContract.getHexParam(0, this).getMiniData();
		
		//Get the Transaction
		Transaction trans = zContract.getTransaction();
		
		//The Total
		MiniNumber total = MiniNumber.ZERO;
		
		//Cycle through the inputs..
		ArrayList<Coin> outputs = trans.getAllOutputs();
		for(Coin cc : outputs) {
			
			if(cc.getTokenID().isEqual(tokenid)) {
				
				if(tokenid.isEqual(Token.TOKENID_MINIMA)) {
					
					//Plain Minima
					total = total.add(cc.getAmount());
					
				}else {
					
					//Get the token..
					Token td = cc.getToken();
					
					//Add the scaled amount..
					total = total.add(td.getScaledTokenAmount(cc.getAmount()));
				}
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
