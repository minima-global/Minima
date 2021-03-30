
package org.minima.system.network.base;

public interface ExceptionThrowingSupplier<O> {
  O get() throws Throwable;
}
