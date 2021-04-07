/**
 * 
 */
package org.minima.kissvm.functions.sha;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
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
		checkExactParamNumber(2);
		
		//The Bit Length
		int bitlength = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
		Value vv = getParameter(1).getValue(zContract);
		checkIsOfType(vv, Value.VALUE_HEX | Value.VALUE_SCRIPT);
		
		byte[] data = null;
		if(vv.getValueType() == Value.VALUE_HEX) {
			//HEX
			HEXValue hex = (HEXValue)vv;
			data = hex.getRawData();
			
		}else {
			//Script..
			ScriptValue scr = (ScriptValue)vv;
			data = scr.getBytes();
			
		}
	
		//Check valid..
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