package org.minima.kissvm.functions.txn.input;

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
public class VERIFYIN extends MinimaFunction{

	public VERIFYIN() {
		super("VERIFYIN");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Which Output
		int input     = getParameter(0).getValue(zContract).getNumber().getAsInt();
		
		//Get the details
		MiniData address  = new MiniData(getParameter(1).getValue(zContract).getRawData());
		MiniNumber amount = getParameter(2).getValue(zContract).getNumber();
		MiniData tokenid  = new MiniData(getParameter(3).getValue(zContract).getRawData());
		
		//Is there a 4th Parameter ?
		int amountchecktype = 0 ;
		if(getParameterNum()>4) {
			amountchecktype = getParameter(4).getValue(zContract).getNumber().getAsInt();
		}
		
		//Check an output exists..
		Transaction trans = zContract.getTransaction();
	
		//Check output exists..
		ArrayList<Coin> ins = trans.getAllInputs();
		if(ins.size()<=input) {
			throw new ExecutionException("Input number too high "+input+"/"+ins.size());
		}
		
		//Get it..
		Coin cc = ins.get(input);
		
		//Now Check
		boolean addr = address.isEqual(cc.getAddress());  
		boolean tok  = tokenid.isEqual(cc.getTokenID());  
		
		//Amount can be 3 type.. EQ, LTE, GTE
		MiniNumber inamt = cc.getAmount();
		
		//Could be a token Amount!
		if(!cc.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
			//Get the Multiple..
			TokenProof td = zContract.getWitness().getTokenDetail(cc.getTokenID());
			inamt = cc.getAmount().mult(td.getScaleFactor());
		}
		
		//Amount can be 3 type.. EQ, LTE, GTE
		boolean amt  = false;
		if(amountchecktype == 0) {
			amt  = inamt.isEqual(amount);
		}else if(amountchecktype == -1) {
			amt  = inamt.isLessEqual(amount);
		}else {
			amt  = inamt.isMoreEqual(amount);
		}
		
		//Return if all true
		return new BooleanValue( addr && amt && tok );
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new VERIFYIN();
	}

}
