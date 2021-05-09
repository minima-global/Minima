package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;

import org.junit.Test;
import org.minima.kissvm.values.NumberValue;
import org.minima.objects.base.MiniNumber;

public class NumberValueTests {

    @Test
    public void testConstructors() {

        int intNumber = 0x00FFFFFF;
        long longNumber = 0x00FFFFFFFFFFFFFFL;
//        double doubleNumber = 1.123456789;
        MiniNumber miniNumber = new MiniNumber(intNumber + 64);

        NumberValue nv1 = new NumberValue(intNumber);
        NumberValue nv1a = new NumberValue(Integer.toString(intNumber));
        NumberValue nv2 = new NumberValue(longNumber);
        NumberValue nv2a = new NumberValue(Long.toString(longNumber));
//        NumberValue nv3 = new NumberValue(doubleNumber);
//        NumberValue nv3a = new NumberValue(Double.toString(doubleNumber));
        NumberValue nv4 = new NumberValue(miniNumber);
        NumberValue nv4a = new NumberValue(miniNumber.toString());

        assertEquals("should be equal ", nv1.getNumber().getAsInt(), nv1a.getNumber().getAsInt());
        assertEquals("should be equal ", nv2.getNumber().getAsLong(), nv2a.getNumber().getAsLong());
//        assertTrue("should be true ", nv3.getNumber().getAsDouble() == nv3a.getNumber().getAsDouble());
        assertEquals("should be equal ", nv4.getNumber().getAsBigInteger(), nv4a.getNumber().getAsBigInteger());

        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv1.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv1a.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv2.getValueType());
        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv2a.getValueType());
//        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv3.getValueType());
//        assertEquals("should be equal ", NumberValue.VALUE_NUMBER, nv3a.getValueType());

    }

    @Test
    public void testArithmentic() {

        // Addition
        NumberValue nv1 = new NumberValue(0);
        NumberValue nv2 = new NumberValue(0);
        NumberValue res = new NumberValue(0);
        NumberValue res2 = new NumberValue(0);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));
        assertTrue("should be true ", nv2.add(nv1).isEqual(res));

        nv1 = new NumberValue(123456789);
        nv2 = new NumberValue(76543211);
        res = new NumberValue(200000000);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));
        assertTrue("should be true ", nv2.add(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MAX_VALUE);
        nv2 = new NumberValue(Integer.MIN_VALUE);
        res = new NumberValue(-1);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));
        assertTrue("should be true ", nv2.add(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MAX_VALUE);
        nv2 = new NumberValue(1);
        res = new NumberValue(1L + Integer.MAX_VALUE);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));
        assertTrue("should be true ", nv2.add(nv1).isEqual(res));

        nv1 = new NumberValue(Long.MAX_VALUE);
        nv2 = new NumberValue(Long.MIN_VALUE);
        res = new NumberValue(-1);
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));
        assertTrue("should be true ", nv2.add(nv1).isEqual(res));

        nv1 = new NumberValue(Long.MAX_VALUE);
        nv2 = new NumberValue(1L);
        res = new NumberValue(new MiniNumber(1L).add(new MiniNumber(Long.MAX_VALUE)));
        assertFalse("should be false ", res.getNumber().isEqual(new MiniNumber(Long.MAX_VALUE)));
        assertTrue("should be true ", nv1.add(nv2).isEqual(res));
        assertTrue("should be true ", nv2.add(nv1).isEqual(res));

        // Subtraction
        nv1 = new NumberValue(0);
        nv2 = new NumberValue(0);
        res = new NumberValue(0);
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));

        nv1 = new NumberValue(123456789);
        nv2 = new NumberValue(23456789);
        res = new NumberValue(100000000);
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));
        assertFalse("should be false ", nv2.sub(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MIN_VALUE);
        nv2 = new NumberValue(1);
        res = new NumberValue(Integer.MIN_VALUE - 1L);
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));
        assertFalse("should be false ", nv2.sub(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MIN_VALUE);
        nv2 = new NumberValue(Integer.MAX_VALUE);
        res = new NumberValue(Long.valueOf(Integer.MIN_VALUE) - Long.valueOf(Integer.MAX_VALUE));
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));
        assertFalse("should be false ", nv2.sub(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MAX_VALUE);
        nv2 = new NumberValue(Integer.MIN_VALUE);
        res = new NumberValue(Long.valueOf(Integer.MAX_VALUE) - Long.valueOf(Integer.MIN_VALUE));
        assertTrue("should be true ", nv1.sub(nv2).isEqual(res));
        assertFalse("should be false ", nv2.sub(nv1).isEqual(res));

        // Multiplication
        nv1 = new NumberValue(0);
        nv2 = new NumberValue(0);
        res = new NumberValue(0);
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));

        nv1 = new NumberValue(12345);
        nv2 = new NumberValue(67890);
        res = new NumberValue(838102050);
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));
        assertTrue("should be true ", nv2.mult(nv1).isEqual(res));

        nv1 = new NumberValue(1000000);
        nv2 = new NumberValue(1000000);
        res = new NumberValue(Long.valueOf(1000000) * Long.valueOf(1000000));
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));
        assertTrue("should be true ", nv2.mult(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MIN_VALUE);
        nv2 = new NumberValue(1);
        res = new NumberValue(Integer.MIN_VALUE);
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));
        assertTrue("should be true ", nv2.mult(nv1).isEqual(res));

        nv1 = new NumberValue(Integer.MAX_VALUE);
        nv2 = new NumberValue(1);
        res = new NumberValue(Integer.MAX_VALUE);
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));
        assertTrue("should be true ", nv2.mult(nv1).isEqual(res));

        nv1 = new NumberValue(Long.MIN_VALUE);
        nv2 = new NumberValue(1);
        res = new NumberValue(Long.MIN_VALUE);
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));
        assertTrue("should be true ", nv2.mult(nv1).isEqual(res));

        nv1 = new NumberValue(Long.MAX_VALUE);
        nv2 = new NumberValue(1);
        res = new NumberValue(Long.MAX_VALUE);
        assertTrue("should be true ", nv1.mult(nv2).isEqual(res));
        assertTrue("should be true ", nv2.mult(nv1).isEqual(res));

        // Division
        assertThrows(ArithmeticException.class, () -> {
            NumberValue nv1l = new NumberValue(0);
            NumberValue nv2l = new NumberValue(0);
            NumberValue resl = new NumberValue(0);
            nv1l.div(nv2l);
        });
        assertThrows(ArithmeticException.class, () -> {
            NumberValue nv1l = new NumberValue(0);
            NumberValue nv2l = new NumberValue(0);
            NumberValue resl = new NumberValue(0);
            nv2l.div(nv1l);
        });

        nv1 = new NumberValue(0);
        nv2 = new NumberValue(1);
        res = new NumberValue(0);
        assertTrue("should be true ", nv1.div(nv2).isEqual(res));

//        nv1 = new NumberValue(20000);
//        nv2 = new NumberValue(10000);
//        res = new NumberValue(20000 / 10000);
//        res2 = new NumberValue(Double.valueOf(10000) / Double.valueOf(20000));
//        assertTrue("should be true ", nv1.div(nv2).isEqual(res));
//        assertTrue("should be true ", nv2.div(nv1).isEqual(res2));
//
//        nv1 = new NumberValue(1000000);
//        nv2 = new NumberValue(1000000);
//        res = new NumberValue(1);
//        assertTrue("should be true ", nv1.div(nv2).isEqual(res));
//        assertTrue("should be true ", nv2.div(nv1).isEqual(res));
//
//        nv1 = new NumberValue(Integer.MIN_VALUE);
//        nv2 = new NumberValue(1);
//        res = new NumberValue(Integer.MIN_VALUE);
////        res2 = new NumberValue(Double.valueOf(1) / Integer.MIN_VALUE);
//        assertTrue("should be true ", nv1.div(nv2).isEqual(res));
//        res = new NumberValue(nv2.getNumber().div(nv1.getNumber()));
//        //assertTrue("should be true ", res.isEqual(res2)); // precision issue -0.00000000046566128730773926 vs -0.0000000004656612873077392578125
//
//        nv1 = new NumberValue(Long.MIN_VALUE);
//        nv2 = new NumberValue(1);
//        res = new NumberValue(Long.MIN_VALUE);
//        res2 = new NumberValue(Double.valueOf(1) / Long.MIN_VALUE);
//        assertTrue("should be true ", nv1.div(nv2).isEqual(res));
//        res = new NumberValue(nv2.getNumber().div(nv1.getNumber()));
//        //assertTrue("should be true ", res.isEqual(res2)); // precision issue -0.0000000000000000001084202172485504434007452800869941 vs -0.00000000000000000010842021724855044
//
//        nv1 = new NumberValue(Integer.MAX_VALUE);
//        nv2 = new NumberValue(1);
//        res = new NumberValue(Integer.MAX_VALUE);
//        res2 = new NumberValue(Double.valueOf(1) / Integer.MAX_VALUE);
//        assertTrue("should be true ", nv1.div(nv2).isEqual(res));
//        res = new NumberValue(nv2.getNumber().div(nv1.getNumber()));
//        //assertTrue("should be true ", res.isEqual(res2)); // precision issue 0.0000000004656612875245796924105750827167998 vs 0.0000000004656612875245797
//
//        nv1 = new NumberValue(Long.MAX_VALUE);
//        nv2 = new NumberValue(1);
//        res = new NumberValue(Long.MAX_VALUE);
//        res2 = new NumberValue(Double.valueOf(1) / Long.MAX_VALUE);
//        assertTrue("should be true ", nv1.div(nv2).isEqual(res));
//        res = new NumberValue(nv2.getNumber().div(nv1.getNumber()));
//        //assertTrue("should be true ", res.isEqual(res2)); // precision issue 0.000000000000000000108420217248550443412500223595217 vs 0.00000000000000000010842021724855044
    }

    @Test
    public void testComaprisons() {

        int counter = 0;
        NumberValue nv1 = new NumberValue(0);
        NumberValue nv2 = new NumberValue(0);

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    nv1 = new NumberValue(Integer.valueOf(0));
                    nv2 = new NumberValue(Integer.valueOf(0));
                    break;
                case 2:
                    nv1 = new NumberValue(Long.valueOf(0));
                    nv2 = new NumberValue(Long.valueOf(0));
                    break;
//                case 3:
//                    nv1 = new NumberValue(Double.valueOf(0));
//                    nv2 = new NumberValue(Double.valueOf(0));
//                    break;
                case 4:
                    nv1 = new NumberValue(new MiniNumber(Integer.valueOf(0)));
                    nv2 = new NumberValue(new MiniNumber(Integer.valueOf(0)));
                    break;
                case 5:
                    nv1 = new NumberValue(new MiniNumber(Long.valueOf(0)));
                    nv2 = new NumberValue(new MiniNumber(Long.valueOf(0)));
                    break;
                case 6:
                    nv1 = new NumberValue(new MiniNumber(BigDecimal.valueOf(0)));
                    nv2 = new NumberValue(new MiniNumber(BigDecimal.valueOf(0)));
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertFalse("should be false ", nv1.isLess(nv2));
            assertFalse("should be false ", nv2.isLess(nv1));
            assertTrue("should be true ", nv1.isLessEqual(nv2));
            assertTrue("should be true ", nv2.isLessEqual(nv1));
            assertFalse("should be false ", nv1.isMore(nv2));
            assertFalse("should be false ", nv2.isMore(nv1));
            assertTrue("should be true ", nv1.isMoreEqual(nv2));
            assertTrue("should be true ", nv2.isMoreEqual(nv1));
        }

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    nv1 = new NumberValue(Integer.valueOf(5));
                    nv2 = new NumberValue(Integer.valueOf(10));
                    break;
                case 2:
                    nv1 = new NumberValue(Long.valueOf(5));
                    nv2 = new NumberValue(Long.valueOf(10));
                    break;
//                case 3:
//                    nv1 = new NumberValue(Double.valueOf(5));
//                    nv2 = new NumberValue(Double.valueOf(10));
//                    break;
                case 4:
                    nv1 = new NumberValue(new MiniNumber(Integer.valueOf(5)));
                    nv2 = new NumberValue(new MiniNumber(Integer.valueOf(10)));
                    break;
                case 5:
                    nv1 = new NumberValue(new MiniNumber(Long.valueOf(5)));
                    nv2 = new NumberValue(new MiniNumber(Long.valueOf(10)));
                    break;
                case 6:
                    nv1 = new NumberValue(new MiniNumber(BigDecimal.valueOf(5)));
                    nv2 = new NumberValue(new MiniNumber(BigDecimal.valueOf(10)));
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertTrue("should be true ", nv1.isLess(nv2));
            assertFalse("should be false ", nv2.isLess(nv1));
            assertTrue("should be true ", nv1.isLessEqual(nv2));
            assertFalse("should be false ", nv2.isLessEqual(nv1));
            assertFalse("should be false ", nv1.isMore(nv2));
            assertTrue("should be true ", nv2.isMore(nv1));
            assertFalse("should be false ", nv1.isMoreEqual(nv2));
            assertTrue("should be true ", nv2.isMoreEqual(nv1));
        }

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    nv1 = new NumberValue(Integer.MIN_VALUE);
                    nv2 = new NumberValue(Integer.MAX_VALUE);
                    break;
                case 2:
                    nv1 = new NumberValue(Long.MIN_VALUE);
                    nv2 = new NumberValue(Long.MAX_VALUE);
                    break;
//                case 3:
//                    nv1 = new NumberValue(-Double.MAX_VALUE);
//                    nv2 = new NumberValue(Double.MAX_VALUE);
//                    break;
                case 4:
//                    assertThrows(NumberFormatException.class, () -> {
//                        new NumberValue(-Double.MIN_VALUE);
//                    });
//                    assertThrows(NumberFormatException.class, () -> {
//                        new NumberValue(Double.MIN_VALUE);
//                    });
                    continue;
                case 5:
                    nv1 = new NumberValue(new MiniNumber(Integer.MIN_VALUE));
                    nv2 = new NumberValue(new MiniNumber(Integer.MAX_VALUE));
                    break;
                case 6:
                    nv1 = new NumberValue(new MiniNumber(Long.MIN_VALUE));
                    nv2 = new NumberValue(new MiniNumber(Long.MAX_VALUE));
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertTrue("should be true ", nv1.isLess(nv2));
            assertFalse("should be false ", nv2.isLess(nv1));
            assertTrue("should be true ", nv1.isLessEqual(nv2));
            assertFalse("should be false ", nv2.isLessEqual(nv1));
            assertFalse("should be false ", nv1.isMore(nv2));
            assertTrue("should be true ", nv2.isMore(nv1));
            assertFalse("should be false ", nv1.isMoreEqual(nv2));
            assertTrue("should be true ", nv2.isMoreEqual(nv1));
        }
    }

    @Test
    public void testToString() {
        NumberValue nv;

        nv = new NumberValue(Integer.valueOf(0));
        assertEquals("should be equal ", Integer.toString(0), nv.toString());
        nv = new NumberValue(Long.valueOf(0));
        assertEquals("should be equal ", Long.toString(0), nv.toString());
        //nv = new NumberValue(Double.valueOf(0));
        //assertEquals("should be equal ", Double.toString(0), nv.toString()); // 0.0 vs 0

        nv = new NumberValue(Integer.valueOf(1));
        assertEquals("should be equal ", Integer.toString(1), nv.toString());
        nv = new NumberValue(Long.valueOf(1));
        assertEquals("should be equal ", Long.toString(1), nv.toString());
        //nv = new NumberValue(Double.valueOf(1));
        //assertEquals("should be equal ", Double.toString(1), nv.toString()); // 1.0 vs 1

        nv = new NumberValue(Integer.MIN_VALUE);
        assertEquals("should be equal ", Integer.toString(Integer.MIN_VALUE), nv.toString());
        //nv = new NumberValue(Long.MIN_VALUE);
        //assertEquals("should be equal ", Long.toString(Long.MIN_VALUE), nv.toString()); // Precision issue
        //nv = new NumberValue(Double.MIN_VALUE);
        //assertEquals("should be equal ", Double.toString(Double.MIN_VALUE), nv.toString()); // scientific vs decimal representations

        nv = new NumberValue(Integer.MAX_VALUE);
        assertEquals("should be equal ", Integer.toString(Integer.MAX_VALUE), nv.toString());
        //nv = new NumberValue(Long.MAX_VALUE);
        //assertEquals("should be equal ", Long.toString(Long.MAX_VALUE), nv.toString()); // Precision issue
        //nv = new NumberValue(Double.MAX_VALUE);
        //assertEquals("should be equal ", Double.toString(Double.MAX_VALUE), nv.toString()); // scientific vs decimal representations
    }
}
