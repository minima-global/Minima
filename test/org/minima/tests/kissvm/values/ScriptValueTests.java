package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import org.junit.Test;
import org.minima.kissvm.values.ScriptValue;

public class ScriptValueTests {

    @Test
    public void testConstructors() {

    	assertThrows(IllegalArgumentException.class, () -> {
    		ScriptValue sv1 = new ScriptValue(""); // receives empty string!!!
        });
        
        ScriptValue sv2 = new ScriptValue("[]"); // receives empty script!!!
        ScriptValue sv3 = new ScriptValue("[RETURN TRUE]"); // receives valid script
        ScriptValue sv4 = new ScriptValue("[HELLO WORLD]"); // receives invalid script!!!

//        assertEquals("should be equal ", "", sv1.toString());
        assertEquals("should be equal ", "[ ]", sv2.toString()); // adds space inside brackets
        assertEquals("should be equal ", "[ RETURN TRUE ]", sv3.toString()); // adds leading and trailing space to script text
        assertEquals("should be equal ", "[ hello world ]", sv4.toString()); // converts all non keywords to lowercase

        assertEquals("should be equal ", "[ RETURN TRUE hello world ]", sv3.add(sv4).toString()); // does not remove square brackets on concat
        assertEquals("should be equal ", "[ hello world RETURN TRUE ]", sv4.add(sv3).toString()); // does not remove square brackets on concat

//        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv1.getValueType());
        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv2.getValueType());
        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv3.getValueType());
        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, sv4.getValueType());

    }
}
