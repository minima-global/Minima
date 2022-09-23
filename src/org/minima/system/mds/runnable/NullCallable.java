package org.minima.system.mds.runnable;

import org.mozilla.javascript.Callable;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Scriptable;

/**
 * Required to create the Native JSON in RHINO
 */
public class NullCallable implements Callable{
    @Override
    public Object call(Context context, Scriptable scope, Scriptable holdable, Object[] objects){
        return objects[1];
    }
}