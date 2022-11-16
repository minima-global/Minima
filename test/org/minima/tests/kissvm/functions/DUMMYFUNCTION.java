package org.minima.tests.kissvm.functions;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

public class DUMMYFUNCTION extends MinimaFunction {

    public DUMMYFUNCTION() {
        super("DUMMYFUNCTION");
    }

    @Override
    public Value runFunction(Contract zContract) throws ExecutionException {
        return new BooleanValue(true);
    }

    @Override
    public MinimaFunction getNewFunction() {
        return new DUMMYFUNCTION();
    }

    @Override
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
    
	@Override
	public int requiredParams() {
		return 1;
	}
}
