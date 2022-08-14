package org.minima.kissvm.functions.txn.output;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

/**
 * Verify that the specified output exists in the transaction.
 * @author spartacusrex
 *
 */
public class VERIFYOUT extends MinimaFunction{

	public VERIFYOUT() {
		super("VERIFYOUT");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Which Output
		int output = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
		//Get the details
		MiniData address  = new MiniData(zContract.getHexParam(1, this).getRawData());
		MiniNumber amount = zContract.getNumberParam(2, this).getNumber();
		MiniData tokenid  = new MiniData(zContract.getHexParam(3, this).getRawData());
		boolean keepstate = zContract.getBoolParam(4, this).isTrue();
		
		//Check an output exists..
		Transaction trans = zContract.getTransaction();
		
		//Check output exists..
		ArrayList<Coin> outs = trans.getAllOutputs();
		if(output<0 || outs.size()<=output) {
			throw new ExecutionException("Output out of range "+output+"/"+outs.size());
		}
		
		//Get it..
		Coin cc = outs.get(output);
		
		//Check Keep State
		if(cc.storeState() != keepstate) {
			return BooleanValue.FALSE;
		}
		
		//Now Check
		boolean addr = address.isEqual(cc.getAddress());  
		boolean tok  = tokenid.isEqual(cc.getTokenID());  
		
		//The amount may need to be scaled
		MiniNumber outamt = cc.getAmount();
		
		//Could be a token Amount!
		if(!cc.getTokenID().isEqual(Token.TOKENID_MINIMA)) {
			//Get the token details
			Token cctok = cc.getToken();
			if(cctok == null) {
				throw new ExecutionException("No token specified @ Output coin "+output+" "+cc.getTokenID());
			}
			
			//Scale the amount
			outamt = cctok.getScaledTokenAmount(cc.getAmount());
		}
		
		//Are they equal
		boolean amt  = outamt.isEqual(amount);
		
		//If all equal..
		boolean ver = addr && amt && tok;
		
		//Return if all true
		return new BooleanValue( ver );
	}
	
	@Override
	public int requiredParams() {
		return 5;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new VERIFYOUT();
	}

}
