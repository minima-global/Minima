package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

public class ScriptValueTests {

    @Test
    public void testConstructors() {

        ScriptValue sv1 = new ScriptValue("");
        ScriptValue sv2 = new ScriptValue("[]");
        ScriptValue sv3 = new ScriptValue("return true");
        ScriptValue sv4 = new ScriptValue("HELLO WORLD");

        assertEquals("should be equal ", "HELLO WORLD", Value.getValue("[HELLO WORLD]").toString());
        assertEquals("should be equal ", "[]", sv2.toString());
        assertEquals("should be equal ", "return true", sv3.toString());
        assertEquals("should be equal ", "HELLO WORLD", sv4.toString());

        assertEquals("should be equal ", "return trueHELLO WORLD", sv3.add(sv4).toString());
        assertEquals("should be equal ", "HELLO WORLDreturn true", sv4.add(sv3).toString());

        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv1.getValueType());
        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv2.getValueType());
        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv3.getValueType());
        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv4.getValueType());

    }
}
