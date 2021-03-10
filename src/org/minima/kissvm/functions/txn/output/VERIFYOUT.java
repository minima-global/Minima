package org.minima.kissvm.functions.txn.output;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;

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
		if(getAllParameters().size()>5) {
			throw new ExecutionException("Too many parameters for VERIFYOUT");
		}
		
		//Which Output
		int output = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
		//Get the details
		MiniData address  = new MiniData(zContract.getHEXParam(1, this).getRawData());
		MiniNumber amount = zContract.getNumberParam(2, this).getNumber();
		MiniData tokenid  = new MiniData(zContract.getHEXParam(3, this).getRawData());
		
		//Is there a 4th Parameter ?
		int amountchecktype = 0 ;
		if(getParameterNum()>4) {
			amountchecktype = zContract.getNumberParam(4, this).getNumber().getAsInt();
		}
			
		//Check an output exists..
		Transaction trans = zContract.getTransaction();
	
		//Check output exists..
		ArrayList<Coin> outs = trans.getAllOutputs();
		if(outs.size()<=output) {
			return new BooleanValue( false );
		}
		
		//Get it..
		Coin cc = outs.get(output);
		
		//Now Check
		boolean addr = address.isEqual(cc.getAddress());  
		boolean tok  = tokenid.isEqual(cc.getTokenID());  
		
		//Amount can be 3 type.. EQ, LTE, GTE
		MiniNumber outamt = cc.getAmount();
		
		//Could be a token Amount!
		if(!cc.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
			//Get the Multiple..
			TokenProof td = zContract.getWitness().getTokenDetail(cc.getTokenID());
			if(td == null) {
				throw new ExecutionException("No Token found for ID "+cc.getTokenID());
			}
			
			outamt = cc.getAmount().mult(td.getScaleFactor());
		}
		
		boolean amt  = false;
		if(amountchecktype == 0) {
			amt  = outamt.isEqual(amount);
		}else if(amountchecktype == -1) {
			amt  = outamt.isLessEqual(amount);
		}else {
			amt  = outamt.isMoreEqual(amount);
		}
		
		boolean ver = addr && amt && tok;
		
		//Return if all true
		return new BooleanValue( ver );
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new VERIFYOUT();
	}

}
