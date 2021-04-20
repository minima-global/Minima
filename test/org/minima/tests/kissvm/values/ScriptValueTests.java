package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

public class ScriptValueTests {

    @Test
    public void testConstructors() {

        StringValue sv1 = new StringValue("");
        StringValue sv2 = new StringValue("[]");
        StringValue sv3 = new StringValue("return true");
        StringValue sv4 = new StringValue("HELLO WORLD");

        assertEquals("should be equal ", "HELLO WORLD", Value.getValue("[HELLO WORLD]").toString());
        assertEquals("should be equal ", "[]", sv2.toString());
        assertEquals("should be equal ", "return true", sv3.toString());
        assertEquals("should be equal ", "HELLO WORLD", sv4.toString());

        assertEquals("should be equal ", "return trueHELLO WORLD", sv3.add(sv4).toString());
        assertEquals("should be equal ", "HELLO WORLDreturn true", sv4.add(sv3).toString());

        assertEquals("should be equal ", StringValue.VALUE_SCRIPT, sv1.getValueType());
        assertEquals("should be equal ", StringValue.VALUE_SCRIPT, sv2.getValueType());
        assertEquals("should be equal ", StringValue.VALUE_SCRIPT, sv3.getValueType());
        assertEquals("should be equal ", StringValue.VALUE_SCRIPT, sv4.getValueType());

    }
}
