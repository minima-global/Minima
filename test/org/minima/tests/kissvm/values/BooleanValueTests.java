package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.kissvm.values.BooleanValue;

public class BooleanValueTests {

    @Test
    public void testConstructors() {
        BooleanValue bvt = new BooleanValue(true);
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, bvt.getValueType());
        assertEquals("should be equal ", "TRUE", bvt.toString());

        bvt = BooleanValue.TRUE;
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, bvt.getValueType());
        assertEquals("should be equal ", "TRUE", bvt.toString());

        BooleanValue bvf = new BooleanValue(false);
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, bvf.getValueType());
        assertEquals("should be equal ", "FALSE", bvf.toString());

        bvf = BooleanValue.FALSE;
        assertEquals("should be equal ", BooleanValue.VALUE_BOOLEAN, bvf.getValueType());
        assertEquals("should be equal ", "FALSE", bvf.toString());
    }

    @Test
    public void testGettersAndSetters() {
        BooleanValue bvt1 = new BooleanValue(true);
        BooleanValue bvt2 = BooleanValue.TRUE;

        BooleanValue bvf1 = new BooleanValue(false);
        BooleanValue bvf2 = BooleanValue.FALSE;

        assertTrue("should be true ", bvt1.isTrue());
        assertTrue("should be true ", bvt2.isTrue());
        assertTrue("should be true ", bvf1.isFalse());
        assertTrue("should be true ", bvf2.isFalse());

        assertTrue("should be true ", bvt1.isEqual(bvt1));
        assertTrue("should be true ", bvt1.isEqual(bvt2));
        assertTrue("should be true ", bvt2.isEqual(bvt1));

        assertFalse("should be false", bvt1.isEqual(bvf1));
        assertFalse("should be false ", bvf2.isEqual(bvt2));

//        assertEquals("should be equal ", MiniNumber.ONE.getAsInt(), bvt1.getNumber().getAsInt());
//        assertEquals("should be equal ", MiniNumber.ZERO.getAsInt(), bvf1.getNumber().getAsInt());
    }

}
