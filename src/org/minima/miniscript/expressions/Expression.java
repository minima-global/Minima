/**
 * 
 */
package org.minima.miniscript.expressions;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public interface Expression {
	public Value getValue(Contract zContract) throws ExecutionException;
}
