package org.minima.miniscript.functions.base;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.Value;

public class GET extends MinimaFunction{

	public GET() {
		super("GET");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//The final Param String to search for
		String ps = "";
		
		//Get all the parameters
		ArrayList<Expression> params = getAllParameters();
		for(Expression exp : params) {
			ps += exp.getValue(zContract).getNumber().toString()+",";		
		}
		
		//Now get this string value from the Contract
		return zContract.getArrayValue(ps);
	}

	@Override
	public MinimaFunction getNewFunction() {
		return new GET();
	}
}
