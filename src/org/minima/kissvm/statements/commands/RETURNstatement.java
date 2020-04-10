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
public class RETURNstatement implements Statement{

	Expression mReturnValue;
	
	/**
	 * 
	 */
	public RETURNstatement(Expression zReturnValue) {
		mReturnValue = zReturnValue;
	}
	
	/* (non-JavadocStat)
	 * @see org.ramcash.ramscript.statements.Statement#execute(org.ramcash.ramscript.Contract)
	 */
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		boolean success = mReturnValue.getValue(zContract).isTrue();
		
		//Trace log
//		zContract.traceLog(toString()+" result:"+success);
		
		//Tell the Contract
		zContract.setRETURNValue(success);
	}
	
	@Override
	public String toString() {
		return "RETURN "+mReturnValue.toString();
	}
}
