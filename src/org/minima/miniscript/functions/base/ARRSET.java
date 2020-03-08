package org.minima.miniscript.functions.base;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

public class ARRSET extends MinimaFunction{

	public ARRSET() {
		super("ARRSET");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//The final Param String to search for
		String ps = "";
		
		//Get all the parameters
		ArrayList<Expression> params = getAllParameters();
		boolean first = true;
		Value tval = null;
		for(Expression exp : params) {
			if(first) {
				//This is the value to set
				tval = exp.getValue(zContract);
			}else {
				ps += exp.getValue(zContract).getNumber().toString()+",";	
			}
					
		}
		
		//Now set this string value in the Contract
		
		
		return new NumberValue(0);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new ARRSET();
	}
}
