/**
 * 
 */
package org.minima.miniscript.functions.base;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class STRCAT extends MinimaFunction{

	public STRCAT() {
		super("STRCAT");
	}

	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		
		if(params.size() < 1) {
			throw new ExecutionException("STRCAT requires at least 1 parameter");
		}
			
		//Sum them
		String fullstring = "";
		for(Expression exp : params) {
			fullstring += exp.getValue(zContract).toString()+" ";
		}
		
		return new ScriptValue(fullstring.trim());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new STRCAT();
	}

}
