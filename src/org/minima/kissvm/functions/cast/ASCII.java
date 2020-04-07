package org.minima.kissvm.functions.cast;

import java.nio.charset.Charset;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

/**
 * Convert a HEXVALUE of ASCII text to a script value 
 * @author spartacusrex
 *
 */
public class ASCII extends MinimaFunction{

	public ASCII() {
		super("ASCII");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the ASCII
		HEXValue ascii = (HEXValue) getParameter(0).getValue(zContract);
		
		//Create a string
		String script = new String(ascii.getRawData(),Charset.forName("US-ASCII"));
		
		return new ScriptValue(script);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new ASCII();
	}

}
