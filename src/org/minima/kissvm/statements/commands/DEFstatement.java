/**
 * 
 */
package org.minima.kissvm.statements.commands;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.StringValue;
import org.minima.utils.MinimaLogger;

/**
 * @author Spartacus Rex
 *
 */
public class DEFstatement implements Statement{

	/**
	 * Function Name
	 */
	String 		mFunction;
	
	/**
	 * The Value
	 */
	Expression 	mScript;
	
	/**
	 * The VARIABLE Constructor
	 * 
	 * @param zVariableName
	 * @param zExpression
	 */
	public DEFstatement(String zFunctionName, Expression zScript) {
		mFunction 	= zFunctionName;
		mScript		= zScript; 
	}
	
	@Override
	public void execute(Contract zContract)  throws ExecutionException {
		
		//get the String Value..
		StringValue script = (StringValue) mScript.getValue(zContract);
				
		//
		
	}
	
	@Override
	public String toString() {
		return "DEF "+mFunction+" = "+mScript;
	}
}
