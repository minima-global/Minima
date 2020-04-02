/**
 * 
 */
package org.minima.miniscript.functions.sha;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.base.MiniData;
import org.minima.utils.Crypto;

/**
 * CURRENTLY STILL USES SHA2!
 * 
 * @author Spartacus Rex
 *
 */
public class SHA3 extends MinimaFunction {

	/**
	 * @param zName
	 */
	public SHA3() {
		super("SHA3");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction()
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the Input Data
		byte[] data = getParameter(0).getValue(zContract).getRawData();
		
		//Is there a second parameter
		int bitlength = 512;
		if(getParameterNum()>1) {
			bitlength = getParameter(1).getValue(zContract).getNumber().getAsInt();
		}
		
		if ( bitlength>512 || bitlength<160 || (bitlength%32!=0) ) {
			throw new ExecutionException("Bitlength incompatible with SHA3 "+bitlength);
		}
		
		//Perform the SHA3 Operation
		byte[] ans = Crypto.getInstance().hashData(data,bitlength);
		
		//return the New HEXValue
		return new HEXValue(ans);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SHA3();
	}
}