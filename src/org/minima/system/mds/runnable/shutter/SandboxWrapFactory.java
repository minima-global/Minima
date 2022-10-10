package org.minima.system.mds.runnable.shutter;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.WrapFactory;

public class SandboxWrapFactory extends WrapFactory {
	@Override
	public Scriptable wrapAsJavaObject(Context cx, Scriptable scope, Object javaObject, Class staticType) {
		return new SandboxNativeJavaObject(scope, javaObject, staticType);
	}
}
