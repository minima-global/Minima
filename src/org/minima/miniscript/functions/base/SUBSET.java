package org.minima.miniscript.functions.base;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;

public class SUBSET extends MinimaFunction {

	public SUBSET() {
		super("SUBSET");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//String or HEX
		int type = getParameter(2).getValue(zContract).getValueType();
		
		//Get a a subset of a hex value..
		int start = getParameter(0).getValue(zContract).getNumber().getAsInt();
		int end   = getParameter(1).getValue(zContract).getNumber().getAsInt();
		int len   = end - start;
		
		//Now pick it out of the 3rd value..
		byte[] orig = getParameter(2).getValue(zContract).getRawData();
		
		//Now get the subset
		byte[] subs = new byte[len];
		System.arraycopy(orig, start, subs, 0, len);
		
		if(type == HEXValue.VALUE_HEX) {
			return new HEXValue(subs);	
		
		}else if(type == ScriptValue.VALUE_SCRIPT) {
			return new ScriptValue(new String(subs));	
		
		}else {
			throw new ExecutionException("Invaid Value Type in SUBSET "+type+") "+getParameter(2).toString());
		}
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new SUBSET();
	}
}
