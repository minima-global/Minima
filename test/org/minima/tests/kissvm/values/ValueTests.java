package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import org.junit.Test;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

public class ValueTests {

    @Test
    public void testGettersAndSetters() {
        assertEquals("should be equal ", new ScriptValue("return true").toString(), Value.getValue("[ RETURN TRUE ]").toString());
        assertEquals("should be equal ", new HEXValue("0xFFFF").toString(), Value.getValue("0xFFFF").toString());
        assertEquals("should be equal ", BooleanValue.TRUE, Value.getValue("TRUE"));
        assertEquals("should be equal ", BooleanValue.FALSE, Value.getValue("FALSE"));
        //assertEquals("should be equal ", BooleanValue.TRUE, Value.getValue("true")); // case sensitive
        //assertEquals("should be equal ", BooleanValue.FALSE, Value.getValue("false")); // case sensitive
        assertEquals("should be equal ", new NumberValue(5).toString(), Value.getValue("5").toString());
        //assertEquals("should be equal ", new NumberValue("!"), Value.getValue("!")); // does not handle invalid input
        //assertEquals("should be equal ", new NumberValue("z"), Value.getValue("z")); // does not handle invalid input

        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, Value.getValueType("[RETURN TRUE]"));
        assertEquals("should be equal ", HEXValue.VALUE_HEX, Value.getValueType("0xFFFF"));
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("TRUE"));
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("FALSE"));
        //assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("true")); // case sensitive
        //assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("false")); // case sensitive
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, Value.getValueType("5"));
        assertThrows(IllegalArgumentException.class, () -> {
        	Value.getValueType("!");
        });
        assertThrows(IllegalArgumentException.class, () -> {
        	Value.getValueType("z");
        });
        
//        assertEquals("should be equal ", -99, Value.getValueType("!")); // handles invalid input
//        assertEquals("should be equal ", -99, Value.getValueType("z")); // handles invalid input

    }
}
