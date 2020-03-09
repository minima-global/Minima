/**
 * 
 */
package org.minima.miniscript.statements;

import java.util.List;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;

/**
 * @author Spartacus Rex
 * 
 * This class represents a single block of code, a list of Statements.
 */
public class StatementBlock {
	/**
	 * All the statements in this block. This includes the child 
	 * statements in IF THEN clauses.
	 */
	List<Statement> mStatements;
	
	/**
	 * Initialise the class with a list of statements
	 * @param zStatements
	 */
	public StatementBlock(List<Statement> zStatements) {
		mStatements = zStatements;
	}
	
	/**
	 * Run the list of statements
	 * 
	 * @param zContract
	 * @throws ExecutionException
	 */
	public void run(Contract zContract) throws ExecutionException {
		//Cycle through all the statements
		for(Statement stat : mStatements) {
			//Check for EXIT
			if(zContract.isSuccessSet()) {
				return;
			}
			
			//This action counts as one instruction
			zContract.incrementInstructions();
			
			//Run the next Statement
			stat.execute(zContract);
		}
	}
}
