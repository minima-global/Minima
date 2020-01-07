package org.minima.miniscript.functions.sigs;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.Value;

public class MULTISIG extends MinimaFunction {

	public MULTISIG() {
		super("MULTISIG");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//How many required.. 
		int num = getParameter(0).getValue(zContract).getNumber().getAsInt();
		
		//How many to check from
		int tot= getParameterNum()-1;
		
		//Cycle..
		int found =0;
		for(int i=0;i<tot;i++) {
			Value sig = getParameter(1+i).getValue(zContract);
		
			if(zContract.checkSignature(sig)) {
				found++;
			}
		}
		
		if(found >= num) {
			return BooleanValue.TRUE;
		}else {
			return BooleanValue.FALSE;
		}
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new MULTISIG();
	}
}
