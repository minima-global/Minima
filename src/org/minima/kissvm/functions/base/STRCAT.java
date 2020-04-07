/**
 * 
 */
package org.minima.kissvm.functions.base;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

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
