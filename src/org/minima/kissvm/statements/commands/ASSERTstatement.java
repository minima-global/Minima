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
public class ASSERTstatement implements Statement{

	Expression mAssertValue;
	
	/**
	 * 
	 */
	public ASSERTstatement(Expression zAssertValue) {
		mAssertValue = zAssertValue;
	}
	
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		//Get the expression
		Value val = mAssertValue.getValue(zContract);
		
		//MUST be a boolean
		if(val.getValueType() != Value.VALUE_BOOLEAN) {
			throw new ExecutionException("ASSERT MUST use a BOOLEAN expression : "+toString());
		}
		
		//Does it pass
		boolean success = ((BooleanValue)val).isTrue();
		
		//Tell the Contract to FAIL if FALSE
		if(!success) {
			zContract.setRETURNValue(false);
		}
	}
	
	@Override
	public String toString() {
		return "ASSERT "+mAssertValue.toString();
	}
}
