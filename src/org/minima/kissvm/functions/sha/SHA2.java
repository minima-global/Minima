package org.minima.kissvm.functions.sha;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;
import org.minima.utils.Crypto;

public class SHA2 extends MinimaFunction {

	/**
	 * @param zName
	 */
	public SHA2() {
		super("SHA2");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction()
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the Input Data
		byte[] data = getParameter(0).getValue(zContract).getRawData();
		
		//Perform the SHA2 Operation
		byte[] ans = Crypto.getInstance().hashSHA2(data);
		
		//Ensure a 32 byte hash
		MiniData hash = new MiniData(ans);
		
		//return the New HEXValue
		return new HEXValue(hash.getData());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SHA2();
	}
}
