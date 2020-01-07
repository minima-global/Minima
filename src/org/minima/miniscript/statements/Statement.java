package org.minima.miniscript.statements;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;

public interface Statement {
	/**
	 * Execute the Statement in the given Contract Environment
	 * 
	 * @param zContract
	 * @return The block of code to execute or null
	 */
	public void execute(Contract zContract) throws ExecutionException;
}
