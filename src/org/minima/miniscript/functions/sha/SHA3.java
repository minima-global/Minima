/**
 * 
 */
package org.minima.miniscript.functions.sha;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.base.MiniData32;
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
		
		//Perform the SHA3 Operation
		byte[] ans = Crypto.getInstance().hashData(data);
		
		//Ensure a 32 byte hash
		MiniData32 hash = new MiniData32(ans);
		
		//return the New HEXValue
		return new HEXValue(hash.getData());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SHA3();
	}
}