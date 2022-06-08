package org.minima.kissvm.functions.sha;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.utils.Crypto;

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
		checkExactParamNumber(requiredParams());
		
		Value vv = getParameter(0).getValue(zContract);
		checkIsOfType(vv, Value.VALUE_HEX | Value.VALUE_SCRIPT);
		
		byte[] data = null;
		if(vv.getValueType() == Value.VALUE_HEX) {
			//HEX
			HexValue hex = (HexValue)vv;
			data = hex.getRawData();
			
		}else {
			//Script..
			StringValue scr = (StringValue)vv;
			data = scr.getBytes();
			
		}
		
		//Perform the SHA2 Operation
		byte[] ans = Crypto.getInstance().hashSHA3(data);
		
		//return the New HEXValue
		return new HexValue(ans);
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SHA3();
	}
}
