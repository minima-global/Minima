package org.minima.kissvm.functions.txn.input;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;

public class GETINAMT extends MinimaFunction {

	public GETINAMT() {
		super("GETINAMT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Which Output
		int input = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
		//Get the Transaction
		Transaction trans = zContract.getTransaction();
		
		//Check output exists..
		ArrayList<Coin> ins = trans.getAllInputs();
		if(input<0 || ins.size()<=input) {
			throw new ExecutionException("Input number out of range "+input+"/"+ins.size());
		}
		
		//Get it..
		Coin cc = ins.get(input);
		
		//Is it a Token..
		if(!cc.getTokenID().isEqual(Token.TOKENID_MINIMA)) {
			//Get the Multiple..
			Token td = cc.getToken();
			if(td == null) {
				throw new ExecutionException("No Token for Input Coin @ "+input+" "+cc.getToken());
			}
			
			return new NumberValue(td.getScaledTokenAmount(cc.getAmount()));
		}
		
		//Return the Amount
		return new NumberValue(cc.getAmount());
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new GETINAMT();
	}
}
