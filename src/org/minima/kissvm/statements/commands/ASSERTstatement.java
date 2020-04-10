/**
 * 
 */
package org.minima.kissvm.statements.commands;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;

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
		//zContract.traceLog(toString()+" result:"+success);
		
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
