package org.minima.kissvm.functions.maths;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.Value;

public class BITSET extends MinimaFunction {

	/**
	 * @param zName
	 */
	public BITSET() {
		super("BITSET");
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
		
		//Set to ON or OFF
		boolean set = getParameter(2).getValue(zContract).isTrue();
		
		//find the byte you need..bit
		int reqbyte = (int)Math.floor(bit / 8);
		
		//Check for Overflow
		if(reqbyte >= datalen) {
			throw new ExecutionException("Overflow Bit set. Total Bytes "+datalen+". Requested "+reqbyte);
		}
		 
		//Which bit in the byte
		int bitbyte = bit - (reqbyte * 8);  
		
		//Copy the array
		byte[] copy = new byte[datalen];
		for(int i=0;i<datalen;i++) {
			copy[i] = data[i];
			
			if(i == reqbyte) {
				if(set) {
					//Set the Bit
					copy[i] |=  (1 << bitbyte);	
				}else{
					//Clear the bit
					copy[i] &= ~(1 << bitbyte);	
				}	
			}
		}
		
		//return the New HEXValue
		return new HEXValue(copy);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new BITSET();
	}
}
