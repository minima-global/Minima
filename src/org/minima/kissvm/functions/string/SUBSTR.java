package org.minima.kissvm.functions.string;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

/**
 * Works on Scripts and HEX
 * @author spartacusrex
 *
 */
public class SUBSTR extends MinimaFunction {

	public SUBSTR() {
		super("SUBSTR");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get a a subset of a hex value..
		int start = zContract.getNumberParam(0, this).getNumber().getAsInt();
		int end   = zContract.getNumberParam(1, this).getNumber().getAsInt();
		int len   = end - start;
		if(len<0) {
			throw new ExecutionException("Negative SUBSTR length "+len);
		}
		
		//Now pick it out of the 3rd value..
		StringValue str = zContract.getStringParam(2, this);
		String main 	= str.toString();
		
		//Check limits
		if(start<0 || end>main.length()) {
			throw new ExecutionException("SUBSTR range outside size of String "+start+"-"+end+" length:"+main.length());
		}
		
		//Now get the substr
		String substr = main.substring(start, end);  
		
		return new StringValue(substr);	
	}
	
	@Override
	public int requiredParams() {
		return 3;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SUBSTR();
	}
}
