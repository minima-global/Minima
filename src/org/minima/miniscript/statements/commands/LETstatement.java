/**
 * 
 */
package org.minima.miniscript.statements.commands;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.statements.Statement;
import org.minima.miniscript.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class LETstatement implements Statement{

	String 		mName;
	Expression 	mValue;
	
	public LETstatement(String zVariableName, Expression zExpression) {
		mName  = zVariableName;
		mValue = zExpression; 
	}
	
	@Override
	public void execute(Contract zContract)  throws ExecutionException {
		//Trace log
		zContract.traceLog(toString());
		
		//Do it..
		zContract.setVariable(mName, mValue.getValue(zContract));
	}
	
	@Override
	public String toString() {
		return "LET "+mName+" = "+mValue;
	}
}
