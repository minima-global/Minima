package org.minima.miniscript.functions.txn;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.txn.input.VERIFYIN;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;

public class SUMINTOK  extends MinimaFunction{

	public SUMINTOK() {
		super("SUMINTOK");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Which token to check for
		MiniData32 token = new MiniData32(getParameter(0).getValue(zContract).getRawData()); 
		
		//Check an output exists..
		Transaction trans = zContract.getTransaction();
	
		//Check output exists..
		ArrayList<Coin> ins = trans.getAllInputs();
		
		//Cycle
		MiniNumber total = MiniNumber.ZERO;
		for(Coin cc : ins) {
			//Check is the correct Token..
			if(cc.getTokenID().isExactlyEqual(token)) {
				total = total.add(cc.getAmount());
			}
		}
		
		//Return if all true
		return new NumberValue( total );
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SUMINTOK();
	}
}