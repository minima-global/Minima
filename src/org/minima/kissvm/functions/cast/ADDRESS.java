package org.minima.kissvm.functions.cast;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;

public class ADDRESS extends MinimaFunction{

	public ADDRESS() {
		super("ADDRESS");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get the Value..
		StringValue str = zContract.getStringParam(0, this);
		
		//Convert  to an address
		Address addr = new Address(str.toString());
		
		return new HexValue(addr.getAddressData());
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new ADDRESS();
	}
}
