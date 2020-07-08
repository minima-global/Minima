package org.minima.system.network.minidapps.minilib;

import org.mozilla.javascript.ScriptableObject;

public class Counter extends ScriptableObject {
    private static final long serialVersionUID = 438270592527335642L;

    // The zero-argument constructor used by Rhino runtime to create instances
    public Counter() { }

    // Method jsConstructor defines the JavaScript constructor
    public void jsConstructor(int a) { count = a; }

    // The class name is defined by the getClassName method
    @Override
    public String getClassName() { return "Counter"; }

    // The method jsGet_count defines the count property.
    public int jsGet_count() { return count++; }

    // Methods can be defined using the jsFunction_ prefix. Here we define
    //  resetCount for JavaScript.
    public void jsFunction_resetCount() { count = 0; }

    private int count;
}
