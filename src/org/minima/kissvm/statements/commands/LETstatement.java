/**
 * 
 */
package org.minima.kissvm.statements.commands;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;

/**
 * @author Spartacus Rex
 *
 */
public class LETstatement implements Statement{

	/**
	 * Which type of LET statement is it.
	 */
	public static int LET_VARIABLE = 0;
	public static int LET_ARRAY    = 1;
	
	int mLETType;
	
	/**
	 * The Array of array params
	 */
	ArrayList<Expression> mArrayPos;
	
	/**
	 * Variable Name
	 */
	String 		mName;
	
	/**
	 * The Value
	 */
	Expression 	mValue;
	
	/**
	 * The VARIABLE Constructor
	 * 
	 * @param zVariableName
	 * @param zExpression
	 */
	public LETstatement(String zVariableName, Expression zExpression) {
		mLETType = LET_VARIABLE;
		mName    = zVariableName;
		mValue   = zExpression;
	}
	
	/**
	 * The ARRAY constructor
	 * 
	 * @param zArrayPos
	 * @param zExpression
	 */
	public LETstatement(ArrayList<Expression> zArrayPos, Expression zExpression) {
		mLETType  = LET_ARRAY;
		mArrayPos = zArrayPos;
		mValue    = zExpression;
	}
	
	@Override
	public void execute(Contract zContract)  throws ExecutionException {
		if(mLETType == LET_VARIABLE) {
			zContract.setVariable(mName, mValue.getValue(zContract));
		}else {
			//The final array pos
			String pos = "";
			
			//Calculate all the expressions..
			for(Expression exp : mArrayPos) {
				pos += exp.getValue(zContract).toString().trim()+",";
			}
			
			//Now ask the contract to set that array variable..
			zContract.setVariable(pos, mValue.getValue(zContract));
		}
	}
	
	@Override
	public String toString() {
		if(mLETType == LET_VARIABLE) {
			return "LET "+mName+" = "+mValue;
		}
		
		//It's an array LET
		String let = "LET ( ";
		for(Expression exp : mArrayPos) {
			let += exp.toString().trim()+" ";
		}
		let = let.trim()+" ) = "+mValue;
		
		return let;
	}
}
