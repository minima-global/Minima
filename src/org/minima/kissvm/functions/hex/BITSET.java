package org.minima.kissvm.functions.hex;

import java.util.BitSet;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
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
		checkExactParamNumber(requiredParams());
		
		//get the Input Data
		byte[] data = zContract.getHexParam(0, this).getRawData();
		int totbits = (data.length * 8) - 1;
		
		//Get the desired Bit
		int bit = zContract.getNumberParam(1, this).getNumber().getAsInt();
		if(bit<0 || bit>totbits) {
			throw new ExecutionException("BitSet too large "+bit+" / "+totbits);
		}
		
		//Set to ON or OFF
		boolean set = zContract.getBoolParam(2, this).isTrue();

		//Create a BitSet object..
		BitSet bits = BitSet.valueOf(data);
		
		//Change the bit
		bits.set(bit, set);
		
		//Now get the new byte array
		byte[] newarray = bits.toByteArray();
		
		//Return the new HEXValue
		return new HexValue(newarray);		
	}
	
	@Override
	public int requiredParams() {
		return 3;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new BITSET();
	}
}
