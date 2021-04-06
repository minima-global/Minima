package org.minima.kissvm.statements.commands;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

/**
 * WHILE..DO..ENDWHILE
 * @author spartacusrex
 *
 */
public class WHILEstatement implements Statement {

	
	/**
	 * This expression must return true to continue..
	 */
	Expression mWhileCheck;
	
	/**
	 * The block of code that is executed..
	 */
	StatementBlock mWhileBlock;
	
	
	public WHILEstatement(Expression zWhileCheck, StatementBlock zCodeBlock) {
		mWhileCheck = zWhileCheck;
		mWhileBlock = zCodeBlock;
	}
	
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		//Calculate the value..
		Value val = mWhileCheck.getValue(zContract);
		
		//MUST be a boolean
		if(val.getValueType() != Value.VALUE_BOOLEAN) {
			throw new ExecutionException("RETURN MUST use a BOOLEAN expression : "+toString());
		}
		
		//Check it..
		BooleanValue bval = (BooleanValue)val;
		
		//Check the expression - this loop wil end when the number of execution points is over 1000
		while(bval.isTrue()) {
			//Run the code..
			mWhileBlock.run(zContract);
			
			//Check for EXIT
			if(zContract.isSuccessSet()) {
				return;
			}
			
			//Calculate the value..
			val = mWhileCheck.getValue(zContract);
			
			//MUST be a boolean
			if(val.getValueType() != Value.VALUE_BOOLEAN) {
				throw new ExecutionException("RETURN MUST use a BOOLEAN expression : "+toString());
			}
			
			//Check it..
			bval = (BooleanValue)val;
		}
	}
	
	@Override
	public String toString() {
		return "WHILE "+mWhileCheck;
	}

}
