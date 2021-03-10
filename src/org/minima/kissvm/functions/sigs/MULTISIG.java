package org.minima.kissvm.functions.sigs;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.Value;

public class MULTISIG extends MinimaFunction {

	public MULTISIG() {
		super("MULTISIG");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkMinParamNumber(2);
		
		//How many required.. 
		int num = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
		//How many to check from
		int tot= getParameterNum()-1;
		
		//Cycle..
		int found =0;
		for(int i=0;i<tot;i++) {
			HEXValue sig = zContract.getHEXParam(1+i, this);
		
			if(zContract.checkSignature(sig)) {
				found++;
			}
			
			if(found >= num) {
				break;
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
