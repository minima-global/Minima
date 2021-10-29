/**
 * 
 */
package org.minima.kissvm.statements.commands;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

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
		//Calculate the value..
		Value val = mReturnValue.getValue(zContract);
		
		//MUST be a boolean
		if(val.getValueType() != Value.VALUE_BOOLEAN) {
			throw new ExecutionException("RETURN MUST use a BOOLEAN expression : "+toString());
		}
		
		//Check it..
		BooleanValue bval = (BooleanValue)val;
		
		//Tell the Contract
		zContract.setRETURNValue(bval.isTrue());
	}
	
	@Override
	public String toString() {
		return "RETURN "+mReturnValue.toString();
	}
}
