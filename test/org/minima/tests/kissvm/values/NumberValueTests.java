package org.minima.tests.kissvm.values;

import org.minima.kissvm.values.NumberValue;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class NumberValueTests {

    @Test
    public void testConstructors() {

        int intNumber = 0x00FFFFFF;
        long longNumber = 0x00FFFFFFFFFFFFFFL;
        double doubleNumber = 1.123456789;
        MiniNumber miniNumber = new MiniNumber(intNumber + 64);

        NumberValue nv1 = new NumberValue(intNumber);
        NumberValue nv1a = new NumberValue(Integer.toString(intNumber));
        NumberValue nv2 = new NumberValue(longNumber);
        NumberValue nv2a = new NumberValue(Long.toString(longNumber));
        NumberValue nv3 = new NumberValue(doubleNumber);
        NumberValue nv3a = new NumberValue(Double.toString(doubleNumber));
        NumberValue nv4 = new NumberValue(miniNumber);
        NumberValue nv4a = new NumberValue(miniNumber.toString());

        assertEquals("should be equal ", nv1.getNumber().getAsInt(), nv1a.getNumber().getAsInt());
        assertEquals("should be equal ", nv2.getNumber().getAsLong(), nv2a.getNumber().getAsLong());
        assertTrue("should be true ", nv3.getNumber().getAsDouble() == nv3a.getNumber().getAsDouble());
        assertEquals("should be equal ", nv4.getNumber().getAsBigInteger(), nv4a.getNumber().getAsBigInteger());

        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv1.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv1a.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv2.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv2a.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv3.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv3a.getValueType());

    }

    @Test
    public void testArithmentic() {

        // Addition
        NumberValue nv1 = new NumberValue(0);
        NumberValue nv2 = new NumberValue(0);
        NumberValue res = new NumberValue(0);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));

        nv1 = new NumberValue(123456789);
        nv2 = new NumberValue(76543211);
        res = new NumberValue(200000000);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));

        nv1 = new NumberValue(Integer.MAX_VALUE);
        nv2 = new NumberValue(Integer.MIN_VALUE);
        res = new NumberValue(-1);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));

        nv1 = new NumberValue(Integer.MAX_VALUE);
        nv2 = new NumberValue(1);
        //res = new NumberValue(1 + Integer.MAX_VALUE); // 32bit integers do not overflow???
        res = new NumberValue(1L + Integer.MAX_VALUE);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));

        //nv1 = new NumberValue(Long.MAX_VALUE);
        //nv2 = new NumberValue(Long.MIN_VALUE);
        //res = new NumberValue(-1);
        //assertTrue("should be true ", nv1.add(nv2).isEqual(res)); // Invalid result
        //nv1 = new NumberValue(Long.MAX_VALUE);
        //nv2 = new NumberValue(1L);
        //res = new NumberValue(1L + Long.MAX_VALUE);
        //assertTrue("should be true ", nv1.add(nv2).isEqual(res)); // 64bit integers do overflow???

        // Subtraction
        nv1 = new NumberValue(0);
        nv2 = new NumberValue(0);
        res = new NumberValue(0);
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));

        nv1 = new NumberValue(123456789);
        nv2 = new NumberValue(23456789);
        res = new NumberValue(100000000);
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));

        nv1 = new NumberValue(Integer.MIN_VALUE);
        nv2 = new NumberValue(1);
        //res = new NumberValue(Integer.MIN_VALUE - 1); // 32bit integers do not overflow???
        res = new NumberValue(Integer.MIN_VALUE - 1L);
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));

        nv1 = new NumberValue(Integer.MIN_VALUE);
        nv2 = new NumberValue(Integer.MAX_VALUE);
        //res = new NumberValue(Integer.MIN_VALUE - Integer.MAX_VALUE); // 32bit integers do not overflow???
        res = new NumberValue(Long.valueOf(Integer.MIN_VALUE) - Long.valueOf(Integer.MAX_VALUE));
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));

    }
}
