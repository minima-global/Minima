package org.minima.kissvm.functions.cast;

import java.nio.charset.Charset;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

/**
 * Replace ALL occurrences of str with replacemnet
 * 
 * @author spartacusrex
 */
public class UTF8 extends MinimaFunction {

	public UTF8() {
		super("UTF8");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get the HEX value
		HexValue hex = zContract.getHexParam(0, this);
		
		//Now create a UTF8 String
		String newstr = new String(hex.getRawData(), Charset.forName("UTF-8"));
		
		return new StringValue(newstr);	
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new UTF8();
	}
}
