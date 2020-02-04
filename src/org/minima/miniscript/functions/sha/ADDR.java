/**
 * 
 */
package org.minima.miniscript.functions.sha;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Address;
import org.minima.objects.base.MiniHash;
import org.minima.utils.Crypto;

/**
 * Convert a ScriptValue into an address
 * 
 * Returns the HEXVale
 * 
 * @author Spartacus Rex
 *
 */
public class ADDR extends MinimaFunction {

	/**
	 * @param zName
	 */
	public ADDR() {
		super("ADDR");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction()
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the Script
		ScriptValue script = (ScriptValue) getParameter(0).getValue(zContract);
		
		//Create an Address
		Address addr = new Address(script.toString());
		
		//return the New HEXValue
		return new HEXValue(addr.getAddressData());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new ADDR();
	}
}