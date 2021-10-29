package org.minima.kissvm.statements;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;

public interface Statement {
	/**
	 * Execute the Statement in the given Contract Environment
	 * 
	 * @param zContract
	 * @return The block of code to execute or null
	 */
	public void execute(Contract zContract) throws ExecutionException;
}
