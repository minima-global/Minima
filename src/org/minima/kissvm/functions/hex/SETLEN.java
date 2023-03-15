package org.minima.kissvm.functions.hex;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;

/**
 * Works on Scripts and HEX
 * @author spartacusrex
 *
 */
public class SETLEN extends MinimaFunction {

	public SETLEN() {
		super("SETLEN");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//What length do you want
		int newlen 	= zContract.getNumberParam(0, this).getNumber().getAsInt();
		if(newlen<1 || newlen>Contract.MAX_DATA_SIZE) {
			throw new ExecutionException("SETLEN size MUST be > 0 and < "+Contract.MAX_DATA_SIZE+" : "+newlen);
		}
		
		//Get the original
		byte[] orig = zContract.getHexParam(1, this).getRawData();
		int origlen = orig.length; 
		
		//Is it already correct
		if(newlen == origlen) {
			return new HexValue(orig);
		}
		
		//Now create the new array
		byte[] newarray = new byte[newlen];
		
		//And copy into it..
		if(origlen > newlen) {
			//Original is longer
			System.arraycopy(orig, origlen-newlen, newarray, 0, newlen);	
		}else {
			//New array is longer
			System.arraycopy(orig, 0, newarray, newlen-origlen, origlen);
		}
		
		return new HexValue(newarray);	
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SETLEN();
	}
}
