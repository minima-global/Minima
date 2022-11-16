/**
 * 
 */
package org.minima.kissvm.statements.commands;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class IFstatement implements Statement {

	/**
	 * A list of all the conditional statements
	 */
	ArrayList<Expression>     mConditions;
	
	/**
	 * A list of the code blocks to be run for each conditional statement
	 */
	ArrayList<StatementBlock> mActions;
	
	
	/**
	 * Main Constructor
	 * 
	 * @param zSingleCondition
	 * @param zSingleCodeBlock
	 * @param zELSECodeBlock
	 */
	public IFstatement() {
		mConditions = new ArrayList<>();
		mActions    = new ArrayList<>();
	}
	
	public void addCondition(Expression zCondition, StatementBlock zCodeBlock) {
		mConditions.add(zCondition);
		mActions.add(zCodeBlock);
	}
	
	/**
	 * Execute this as entire IF statement, including sub routines.
	 */
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		//How many..
		int size = mConditions.size();
		
		//Loop..
		for(int loop=0;loop<size;loop++) {
			//What Condition
			Expression conditional = mConditions.get(loop);
			
			//Calculate the value..
			Value val = conditional.getValue(zContract);
			
			//MUST be a boolean
			if(val.getValueType() != Value.VALUE_BOOLEAN) {
				throw new ExecutionException("IF conditional MUST use a BOOLEAN expression : "+conditional.toString());
			}
			
			//Check it..
			BooleanValue bval = (BooleanValue)val;
			if(bval.isTrue()) {
				//What Action
				StatementBlock codeblock = mActions.get(loop);
				
				//Do it!
				codeblock.run(zContract);
				
				//That's it!
				break;
			}
		}
	}
	
	
	@Override
	public String toString() {
		String ret = "";
		
		//How many..
		int size = mConditions.size();
		
		//Loop..
		for(int loop=0;loop<size;loop++) {
			Expression conditional 		= mConditions.get(loop);
		
			if(loop == 0) {
				ret = "IF "+conditional;
			}else {
				ret += ", ELSEIF "+conditional;
			}
		}
		
		return ret;
	}

}
