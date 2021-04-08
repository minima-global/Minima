package org.minima.kissvm.functions.hex;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;

public class REV extends MinimaFunction {

	public REV() {
		super("REV");
	}	
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//The Data
		HexValue hex = zContract.getHexParam(0, this);
		
		//get the bytes..
		byte[] array  = hex.getRawData();
		
		//Lengths
		int datalen   = array.length;
		
		//Create the reverse buffer
		byte[] revdata = new byte[datalen];
		
		//Reverse..
		for(int i=0;i<datalen; i++){
			revdata[i] = array[array.length -i -1];
		}
		
		// Return reversed value
		return new HexValue(revdata);
	}

	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new REV();
	}
}
