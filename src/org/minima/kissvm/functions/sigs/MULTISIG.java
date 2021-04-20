package org.minima.kissvm.functions.sigs;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;

public class MULTISIG extends MinimaFunction {

	public MULTISIG() {
		super("MULTISIG");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkMinParamNumber(requiredParams());
		
		//How many required.. 
		int num = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
		//How many to check from
		int tot= getParameterNum()-1;
		
		//CHeck valid request..
		if(num<=0) {
			throw new ExecutionException("Must check 1 or more sigs in MULTISIG "+num);
		}
		
		//Cycle..
		int found =0;
		for(int i=0;i<tot;i++) {
			HexValue sig = zContract.getHexParam(1+i, this);
		
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
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new MULTISIG();
	}
}
