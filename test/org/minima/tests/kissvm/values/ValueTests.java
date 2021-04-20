package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

public class ValueTests {

    @Test
    public void testGettersAndSetters() {
        assertEquals("should be equal ", new StringValue(" RETURN TRUE ").toString(), Value.getValue("[ RETURN TRUE ]").toString());
        assertEquals("should be equal ", new HexValue("0xFFFF").toString(), Value.getValue("0xFFFF").toString());
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

        assertEquals("should be equal ", StringValue.VALUE_SCRIPT, Value.getValueType("[RETURN TRUE]"));
        assertEquals("should be equal ", HexValue.VALUE_HEX, Value.getValueType("0xFFFF"));
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("TRUE"));
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, Value.getValueType("FALSE"));
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, Value.getValueType("5"));
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValueType("!");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValueType("z");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValueType("[z");
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.getValueType("!]");
        });

        assertEquals("should be equal ", "BOOLEAN", Value.getValueTypeString(Value.VALUE_BOOLEAN));
        assertEquals("should be equal ", "HEX", Value.getValueTypeString(Value.VALUE_HEX));
        assertEquals("should be equal ", "NUMBER", Value.getValueTypeString(Value.VALUE_NUMBER));
        assertEquals("should be equal ", "SCRIPT", Value.getValueTypeString(Value.VALUE_SCRIPT));
        assertEquals("should be equal ", "ERROR_UNKNOWN_TYPE", Value.getValueTypeString(-99));
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
            (new HexValue("0x00")).verifyType(Value.VALUE_HEX);
        } catch (Exception e) {
            fail();
        }
        assertThrows(IllegalArgumentException.class, () -> {
            (new HexValue("0x00")).verifyType(Value.VALUE_NUMBER);
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
            (new StringValue("Hello World")).verifyType(Value.VALUE_SCRIPT);
        } catch (Exception e) {
            fail();
        }
        assertThrows(IllegalArgumentException.class, () -> {
            (new StringValue("Hello World")).verifyType(Value.VALUE_BOOLEAN);
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
            Value.checkSameType(new HexValue("0x00"), new HexValue("0x00"));
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new HexValue("0x00"), new HexValue("0x00"), Value.VALUE_HEX);
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
            Value.checkSameType(new StringValue("Hello World"), new StringValue("Hello World"));
        } catch (Exception e) {
            fail();
        }
        try {
            Value.checkSameType(new StringValue("Hello World"), new StringValue("Hello World"), Value.VALUE_SCRIPT);
        } catch (Exception e) {
            fail();
        }

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new BooleanValue(true), new HexValue("0x00"));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new BooleanValue(true), new HexValue("0x00"), Value.VALUE_BOOLEAN);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new BooleanValue(true), new BooleanValue(false), Value.VALUE_HEX);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new HexValue("0x00"), new NumberValue(0));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new HexValue("0x00"), new NumberValue(0), Value.VALUE_HEX);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new HexValue("0x00"), new HexValue("0x00"), Value.VALUE_NUMBER);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new NumberValue(0), new StringValue("Hello World"));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new NumberValue(0), new StringValue("Hello World"), Value.VALUE_NUMBER);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new NumberValue(0), new NumberValue(0), Value.VALUE_SCRIPT);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new StringValue("Hello World"), new BooleanValue(true));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new StringValue("Hello World"), new BooleanValue(true), Value.VALUE_SCRIPT);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            Value.checkSameType(new StringValue("Hello World"), new StringValue("Hello World"), Value.VALUE_BOOLEAN);
        });
    }
}
