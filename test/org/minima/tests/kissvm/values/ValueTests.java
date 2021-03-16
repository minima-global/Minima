package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

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
        assertEquals("should be equal ", new NumberValue(5).toString(), Value.getValue("5").toString());
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValue("!]");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValue("[!");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValue("!");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValue("z");
        });

        assertEquals("should be equal ", ScriptValue.VALUE_SCRIPT, Value.getValueType("[RETURN TRUE]"));
        assertEquals("should be equal ", HEXValue.VALUE_HEX, Value.getValueType("0xFFFF"));
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("TRUE"));
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("FALSE"));
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, Value.getValueType("5"));
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValueType("!");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValueType("z");
        });
    }

    @Test
    public void testTypeCheckers() {

        try {
            (new BooleanValue(true)).verifyType(Value.VALUE_BOOLEAN);
        } catch (Exception e) {
            fail();
        }
        assertThrows(IllegalArgumentException.class, () -> {
            (new BooleanValue(true)).verifyType(Value.VALUE_HEX);
        });
        try {
            (new HEXValue("0x00")).verifyType(Value.VALUE_HEX);
        } catch (Exception e) {
            fail();
        }
        assertThrows(IllegalArgumentException.class, () -> {
            (new HEXValue("0x00")).verifyType(Value.VALUE_NUMBER);
        });
        try {
            (new NumberValue(0)).verifyType(Value.VALUE_NUMBER);
        } catch (Exception e) {
            fail();
        }
        assertThrows(IllegalArgumentException.class, () -> {
            (new NumberValue(0)).verifyType(Value.VALUE_SCRIPT);
        });
        try {
            (new ScriptValue("Hello World")).verifyType(Value.VALUE_SCRIPT);
        } catch (Exception e) {
            fail();
        }
        assertThrows(IllegalArgumentException.class, () -> {
            (new ScriptValue("Hello World")).verifyType(Value.VALUE_BOOLEAN);
        });

        try {
            Value.checkSameType(new BooleanValue(true), new BooleanValue(false));
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new BooleanValue(true), new BooleanValue(false), Value.VALUE_BOOLEAN);
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new HEXValue("0x00"), new HEXValue("0x00"));
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new HEXValue("0x00"), new HEXValue("0x00"), Value.VALUE_HEX);
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new NumberValue(0), new NumberValue(0));
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new NumberValue(0), new NumberValue(0), Value.VALUE_NUMBER);
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new ScriptValue("Hello World"), new ScriptValue("Hello World"));
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new ScriptValue("Hello World"), new ScriptValue("Hello World"), Value.VALUE_SCRIPT);
        } catch (Exception e) {
            fail();
        }

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new BooleanValue(true), new HEXValue("0x00"));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new BooleanValue(true), new HEXValue("0x00"), Value.VALUE_BOOLEAN);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new BooleanValue(true), new BooleanValue(false), Value.VALUE_HEX);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new HEXValue("0x00"), new NumberValue(0));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new HEXValue("0x00"), new NumberValue(0), Value.VALUE_HEX);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new HEXValue("0x00"), new HEXValue("0x00"), Value.VALUE_NUMBER);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new NumberValue(0), new ScriptValue("Hello World"));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new NumberValue(0), new ScriptValue("Hello World"), Value.VALUE_NUMBER);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new NumberValue(0), new NumberValue(0), Value.VALUE_SCRIPT);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new ScriptValue("Hello World"), new BooleanValue(true));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new ScriptValue("Hello World"), new BooleanValue(true), Value.VALUE_SCRIPT);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new ScriptValue("Hello World"), new ScriptValue("Hello World"), Value.VALUE_BOOLEAN);
        });
    }
}
