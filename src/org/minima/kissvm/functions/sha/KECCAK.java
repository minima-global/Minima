/**
 * 
 */
package org.minima.kissvm.functions.sha;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.utils.Crypto;

/**
 * CURRENTLY STILL USES SHA2!
 * 
 * @author Spartacus Rex
 *
 */
public class KECCAK extends MinimaFunction {

	/**
	 * @param zName
	 */
	public KECCAK() {
		super("KECCAK");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction()
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
//		//The Bit Length
//		int bitlength = zContract.getNumberParam(0, this).getNumber().getAsInt();
		
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
	
		//Check valid..
//		if ( bitlength>512 || bitlength<160 || (bitlength%32!=0) ) {
//			throw new ExecutionException("Bitlength incompatible with SHA3 "+bitlength);
//		}
		
		//Perform the SHA3 Operation
//		byte[] ans = Crypto.getInstance().hashData(data,bitlength);
		byte[] ans = Crypto.getInstance().hashData(data);
		
		//return the New HEXValue
		return new HexValue(ans);
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new KECCAK();
	}
}