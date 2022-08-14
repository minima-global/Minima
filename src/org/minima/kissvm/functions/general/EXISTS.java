package org.minima.kissvm.functions.general;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

public class EXISTS extends MinimaFunction{

	public EXISTS() {
		super("EXISTS");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkMinParamNumber(requiredParams());
		
		//MUST all be NUMBERS..
		checkAllParamsType(Value.VALUE_NUMBER, zContract);
				
		//The full parameter String to search for
		String ps = "";
		
		//Get all the parameters
		ArrayList<Expression> params = getAllParameters();
		for(Expression exp : params) {
			ps += exp.getValue(zContract).toString().trim()+",";		
		}
		
		//Get the Value.. 
		Value val = zContract.getVariable(ps);
		
		//Does it exist
		if(val == null) {
			return BooleanValue.FALSE;
		}
		
		return BooleanValue.TRUE;
	}

	@Override
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new EXISTS();
	}
}
