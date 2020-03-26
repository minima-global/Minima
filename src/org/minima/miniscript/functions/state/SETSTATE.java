package org.minima.miniscript.functions.state;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;

/**
 * Can ONLY be called once per state per transaction..
 * 
 * @author spartacusrex
 *
 */
public class SETSTATE extends MinimaFunction {

	public SETSTATE() {
		super("SETSTATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Which Output - must be from 0-255
		int statenum = getParameter(0).getValue(zContract).getNumber().getAsInt();
				
		//Get the Transaction
		Transaction trans = zContract.getTransaction();
		
		//Get the state variable..
		if(!trans.stateExists(statenum)) {
			throw new ExecutionException("Invalid State Variable "+statenum);
		}
		
		String stateval = trans.getStateValue(statenum).getData().toString();
		
		//Clean it..
		stateval = Contract.cleanScript(stateval);
		
		//Work it out
		return Value.getValue(stateval);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new SETSTATE();
	}
}
