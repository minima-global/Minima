/**
 * 
 */
package org.minima.kissvm.expressions;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public interface Expression {
	public Value getValue(Contract zContract) throws ExecutionException;
}
