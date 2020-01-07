package org.minima.miniscript.functions.maths;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.BooleanValue;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.base.MiniByte;

public class BITGET extends MinimaFunction {

	/**
	 * @param zName
	 */
	public BITGET() {
		super("BITGET");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction()
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the Input Data
		byte[] data = getParameter(0).getValue(zContract).getRawData();
		int datalen   = data.length;
		
		//Get the desired Bit
		int bit = getParameter(1).getValue(zContract).getNumber().getAsInt();
		
		//find the byte you need..bit
		int reqbyte = (int)Math.floor(bit / 8);
		if(reqbyte >= datalen) {
			throw new ExecutionException("Overflow Bit set. Total Bytes "+datalen+". Requested "+reqbyte);
		}
		
		//Which bit in the byte
		int bitbyte = bit - (reqbyte * 8);  
		
		//Is the bit set or not..
		boolean isSet = (data[reqbyte] & (1 << bitbyte)) != 0;
		
		//return the New HEXValue
		return new BooleanValue(isSet);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new BITGET();
	}
}
