package org.minima.kissvm.functions.base;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

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
