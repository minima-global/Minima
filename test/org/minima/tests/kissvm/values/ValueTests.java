package org.minima.tests.kissvm.values;


import org.junit.jupiter.api.Test;
import org.minima.kissvm.values.*;

import static org.junit.jupiter.api.Assertions.*;

public class ValueTests {

    @Test
    public void testGettersAndSetters() {
        assertEquals(new StringValue(" RETURN TRUE ").toString(), Value.getValue("[ RETURN TRUE ]").toString(), "should be equal ");
        assertEquals(new HexValue("0xFFFF").toString(), Value.getValue("0xFFFF").toString(), "should be equal ");
        assertEquals(BooleanValue.TRUE, Value.getValue("TRUE"), "should be equal ");
        assertEquals(BooleanValue.FALSE, Value.getValue("FALSE"), "should be equal ");
        assertEquals(new NumberValue(5).toString(), Value.getValue("5").toString(), "should be equal ");
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

        assertEquals(StringValue.VALUE_SCRIPT, Value.getValueType("[RETURN TRUE]"), "should be equal ");
        assertEquals(HexValue.VALUE_HEX, Value.getValueType("0xFFFF"), "should be equal ");
        assertEquals(BooleanValue.VALUE_BOOLEAN, Value.getValueType("TRUE"), "should be equal ");
        assertEquals(BooleanValue.VALUE_BOOLEAN, Value.getValueType("FALSE"), "should be equal ");
        assertEquals(NumberValue.VALUE_NUMBER, Value.getValueType("5"), "should be equal ");
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

        assertEquals("BOOLEAN", Value.getValueTypeString(Value.VALUE_BOOLEAN), "should be equal ");
        assertEquals("HEX", Value.getValueTypeString(Value.VALUE_HEX), "should be equal ");
        assertEquals("NUMBER", Value.getValueTypeString(Value.VALUE_NUMBER), "should be equal ");
        assertEquals("SCRIPT", Value.getValueTypeString(Value.VALUE_SCRIPT), "should be equal ");
        assertEquals("ERROR_UNKNOWN_TYPE", Value.getValueTypeString(-99), "should be equal ");
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
