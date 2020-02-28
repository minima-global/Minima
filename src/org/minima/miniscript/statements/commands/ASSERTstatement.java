/**
 * 
 */
package org.minima.miniscript.statements.commands;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.statements.Statement;

/**
 * @author Spartacus Rex
 *
 */
public class ASSERTstatement implements Statement{

	Expression mAssertValue;
	
	/**
	 * 
	 */
	public ASSERTstatement(Expression zAssertValue) {
		mAssertValue = zAssertValue;
	}
	
	/* (non-JavadocStat)
	 * @see org.ramcash.ramscript.statements.Statement#execute(org.ramcash.ramscript.Contract)
	 */
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		boolean success = mAssertValue.getValue(zContract).isTrue();
		
		//Trace log
		zContract.traceLog(toString()+" result:"+success);
		
		//Tell the Contract
		if(!success) {
			zContract.setRETURNValue(false);
		}
	}
	
	@Override
	public String toString() {
		return "ASSERT "+mAssertValue.toString();
	}
}
