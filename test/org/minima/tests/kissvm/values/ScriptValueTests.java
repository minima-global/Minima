package org.minima.tests.kissvm.values;

import org.junit.jupiter.api.Test;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ScriptValueTests {

    @Test
    public void testConstructors() {

        StringValue sv1 = new StringValue("");
        StringValue sv2 = new StringValue("[]");
        StringValue sv3 = new StringValue("return true");
        StringValue sv4 = new StringValue("HELLO WORLD");

        assertEquals("HELLO WORLD", Value.getValue("[HELLO WORLD]").toString(), "should be equal ");
        assertEquals("[]", sv2.toString(), "should be equal ");
        assertEquals("return true", sv3.toString(), "should be equal ");
        assertEquals("HELLO WORLD", sv4.toString(), "should be equal ");

        assertEquals("return trueHELLO WORLD", sv3.add(sv4).toString(), "should be equal ");
        assertEquals("HELLO WORLDreturn true", sv4.add(sv3).toString(), "should be equal ");

        assertEquals(StringValue.VALUE_SCRIPT, sv1.getValueType(), "should be equal ");
        assertEquals(StringValue.VALUE_SCRIPT, sv2.getValueType(), "should be equal ");
        assertEquals(StringValue.VALUE_SCRIPT, sv3.getValueType(), "should be equal ");
        assertEquals(StringValue.VALUE_SCRIPT, sv4.getValueType(), "should be equal ");

    }
}
