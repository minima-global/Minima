package org.minima.miniscript.functions.txn;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;

public class PREVSTATE extends MinimaFunction {

	public PREVSTATE() {
		super("PREVSTATE");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Which Output - must be from 0-255
		int statenum = getParameter(0).getValue(zContract).getNumber().getAsInt();
				
		//Work it out
		return zContract.getPrevState( new MiniNumber(""+statenum));
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new PREVSTATE();
	}
}
