package org.minima.miniscript.statements.commands;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.statements.Statement;
import org.minima.miniscript.statements.StatementBlock;

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
		//Check the expression - this loop wil end when the number of execution points is over 1000
		while(mWhileCheck.getValue(zContract).isTrue()) {
			//Trace log
			zContract.traceLog(toString()+" result:true");
				
			//Run the code..
			mWhileBlock.run(zContract);
		}
		
		//Trace log
		zContract.traceLog(toString()+" result:false");				
	}
	
	@Override
	public String toString() {
		return "WHILE "+mWhileCheck;
	}

}
