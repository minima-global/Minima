package org.minima.tests.kissvm.values;


import org.junit.jupiter.api.Test;
import org.minima.kissvm.values.BooleanValue;

import static org.junit.jupiter.api.Assertions.*;

public class BooleanValueTests {

    @Test
    public void testConstructors() {
        BooleanValue bvt = new BooleanValue(true);
        assertEquals(BooleanValue.VALUE_BOOLEAN, bvt.getValueType(), "should be equal ");
        assertEquals("TRUE", bvt.toString(), "should be equal ");

        bvt = BooleanValue.TRUE;
        assertEquals(BooleanValue.VALUE_BOOLEAN, bvt.getValueType(), "should be equal ");
        assertEquals("TRUE", bvt.toString(), "should be equal ");

        BooleanValue bvf = new BooleanValue(false);
        assertEquals(BooleanValue.VALUE_BOOLEAN, bvf.getValueType(), "should be equal ");
        assertEquals("FALSE", bvf.toString(), "should be equal ");

        bvf = BooleanValue.FALSE;
        assertEquals(BooleanValue.VALUE_BOOLEAN, bvf.getValueType(), "should be equal ");
        assertEquals("FALSE", bvf.toString(), "should be equal ");
    }

    @Test
    public void testGettersAndSetters() {
        BooleanValue bvt1 = new BooleanValue(true);
        BooleanValue bvt2 = BooleanValue.TRUE;

        BooleanValue bvf1 = new BooleanValue(false);
        BooleanValue bvf2 = BooleanValue.FALSE;

        assertTrue(bvt1.isTrue(), "should be true ");
        assertTrue(bvt2.isTrue(), "should be true ");
        assertTrue(bvf1.isFalse(), "should be true ");
        assertTrue(bvf2.isFalse(), "should be true ");

        assertTrue(bvt1.isEqual(bvt1), "should be true ");
        assertTrue(bvt1.isEqual(bvt2), "should be true ");
        assertTrue(bvt2.isEqual(bvt1), "should be true ");

        assertFalse(bvt1.isEqual(bvf1), "should be false");
        assertFalse(bvf2.isEqual(bvt2), "should be false ");

//        assertEquals("should be equal ", MiniNumber.ONE.getAsInt(), bvt1.getNumber().getAsInt());
//        assertEquals("should be equal ", MiniNumber.ZERO.getAsInt(), bvf1.getNumber().getAsInt());
    }

}
