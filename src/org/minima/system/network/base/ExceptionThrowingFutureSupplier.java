
package org.minima.system.network.base;

import java.util.concurrent.CompletionStage;

public interface ExceptionThrowingFutureSupplier<O> {
  CompletionStage<O> get() throws Throwable;
}
