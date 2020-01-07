package org.minima.miniscript.functions.base;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;

public class REV extends MinimaFunction {

	public REV() {
		super("REV");
	}	
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the bytes..
		byte[] array  = getParameter(0).getValue(zContract).getRawData();
		
		//Lengths
		int datalen   = array.length;
		
		//Create the reverse buffer
		byte[] revdata = new byte[datalen];
		
		//Reverse..
		for(int i=0;i<datalen; i++){
			revdata[i] = array[array.length -i -1];
		}
		
		// Return reversed value
		return new HEXValue(revdata);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new REV();
	}
}
