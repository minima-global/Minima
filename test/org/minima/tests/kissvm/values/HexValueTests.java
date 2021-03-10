package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;

import org.junit.Test;
import org.minima.kissvm.values.HEXValue;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class HexValueTests {

    @Test
    public void testConstructors() {

        HEXValue hv1 = new HEXValue(new MiniData());
        HEXValue hv2 = new HEXValue(new MiniData("1234"));

        assertThrows(NumberFormatException.class, () -> {
            new HEXValue(new MiniData("QWER")); //invalid HEX input
        });
        assertThrows(NumberFormatException.class, () -> {
            new HEXValue(new MiniNumber(-1)); // Invalid HEX input
        });
        assertThrows(NumberFormatException.class, () -> {
            new HEXValue(new MiniNumber("1.1")); // Invalid HEX input
        });

        HEXValue hv4 = new HEXValue(new MiniData(new byte[]{}));
        HEXValue hv5 = new HEXValue(new MiniData(new byte[]{(byte) 0x12, (byte) 0x34, (byte) 0x56}));
        HEXValue hv6 = new HEXValue(new MiniNumber(Integer.valueOf(255)));
        HEXValue hv7 = new HEXValue(new MiniNumber(Long.valueOf(65535)));
        HEXValue hv8 = new HEXValue(new MiniNumber(BigDecimal.valueOf(16777215)));
        HEXValue hv9 = new HEXValue(new MiniNumber("1.0"));

        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv1.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv2.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv4.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv5.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv6.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv7.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv8.getValueType());
        assertEquals("should be equal ", HEXValue.VALUE_HEX, hv9.getValueType());
    }

    @Test
    public void testArithmentic() {
        //PADDY - this is not now possible..

//        // Addition
//        HEXValue hv1 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        HEXValue hv2 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        HEXValue res = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        HEXValue res2 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        assertTrue("should be true ", hv1.add(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.add(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.valueOf(123456789)));
//        hv2 = new HEXValue(new MiniNumber(Integer.valueOf(76543211)));
//        res = new HEXValue(new MiniNumber(Integer.valueOf(200000000)));
//        assertTrue("should be true ", hv1.add(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.add(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
////        hv2 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
////        res = new HEXValue(new MiniNumber(-1));
//        //assertTrue("should be true ", nv1.add(nv2).isEqual(res)); // Invalid result
//        //assertTrue("should be true ", nv2.add(nv1).isEqual(res)); // Invalid result
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(1L + Integer.MAX_VALUE));
//        assertTrue("should be true ", hv1.add(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.add(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Long.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(Long.MIN_VALUE));
//        assertThrows(NumberFormatException.class, () -> {
//        	new HEXValue(new MiniNumber(-1)); //Positive numbers only
//        });
////        res = new HEXValue(new MiniNumber(-1));
//        //assertTrue("should be true ", nv1.add(nv2).isEqual(res)); // Invalid result
//        //assertTrue("should be true ", nv2.add(nv1).isEqual(res)); // Invalid result
//
//        hv1 = new HEXValue(new MiniNumber(Long.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1L));
//        res = new HEXValue(new MiniNumber(1L).add(new MiniNumber(Long.MAX_VALUE)));
//        //assertFalse("should be false ", res.getNumber().isEqual(new MiniNumber(Long.MAX_VALUE))); // Invalid arithmetic
//        assertTrue("should be true ", hv1.add(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.add(hv1).isEqual(res));
//
//        // Subtraction
//        hv1 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        hv2 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        res = new HEXValue(new MiniNumber(Integer.valueOf(0)));
//        assertTrue("should be true ", hv1.sub(hv2).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.valueOf(123456789)));
//        hv2 = new HEXValue(new MiniNumber(Integer.valueOf(23456789)));
//        res = new HEXValue(new MiniNumber(Integer.valueOf(100000000)));
//        assertTrue("should be true ", hv1.sub(hv2).isEqual(res));
//        assertFalse("should be false ", hv2.sub(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Integer.MIN_VALUE - 1L));
//        //assertTrue("should be true ", hv1.sub(hv2).isEqual(res)); // Invalid result
//        assertFalse("should be false ", hv2.sub(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        hv2 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        res = new HEXValue(new MiniNumber(Long.valueOf(Integer.MIN_VALUE) - Long.valueOf(Integer.MAX_VALUE)));
//        //assertTrue("should be true ", hv1.sub(hv2).isEqual(res)); // Invalid result
//        assertFalse("should be false ", hv2.sub(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        res = new HEXValue(new MiniNumber(Long.valueOf(Integer.MAX_VALUE) - Long.valueOf(Integer.MIN_VALUE)));
//        //assertTrue("should be true ", hv1.sub(hv2).isEqual(res)); // Invalid result
//        assertFalse("should be false ", hv2.sub(hv1).isEqual(res));
//
//        // Multiplication
//        hv1 = new HEXValue(new MiniNumber(0));
//        hv2 = new HEXValue(new MiniNumber(0));
//        res = new HEXValue(new MiniNumber(0));
//        assertTrue("should be true ", hv1.mult(hv2).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(12345));
//        hv2 = new HEXValue(new MiniNumber(67890));
//        res = new HEXValue(new MiniNumber(838102050));
//        assertTrue("should be true ", hv1.mult(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.mult(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(1000000));
//        hv2 = new HEXValue(new MiniNumber(1000000));
//        res = new HEXValue(new MiniNumber(Long.valueOf(1000000) * Long.valueOf(1000000)));
//        assertTrue("should be true ", hv1.mult(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.mult(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        assertTrue("should be true ", hv1.mult(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.mult(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        assertTrue("should be true ", hv1.mult(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.mult(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Long.MIN_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Long.MIN_VALUE));
//        //assertTrue("should be true ", hv1.mult(hv2).isEqual(res)); // Invalid result
//        //assertTrue("should be true ", hv2.mult(hv1).isEqual(res)); // Invalid result
//
//        hv1 = new HEXValue(new MiniNumber(Long.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Long.MAX_VALUE));
//        assertTrue("should be true ", hv1.mult(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.mult(hv1).isEqual(res));
//
//        // Division
//        assertThrows(ArithmeticException.class, () -> {
//            HEXValue hv1l = new HEXValue(new MiniNumber(0));
//            HEXValue hv2l = new HEXValue(new MiniNumber(0));
//            HEXValue resl = new HEXValue(new MiniNumber(0));
//            hv1l.div(hv2l);
//        });
//        assertThrows(ArithmeticException.class, () -> {
//            HEXValue hv1l = new HEXValue(new MiniNumber(0));
//            HEXValue hv2l = new HEXValue(new MiniNumber(0));
//            HEXValue resl = new HEXValue(new MiniNumber(0));
//            hv2l.div(hv1l);
//        });
//
//        hv1 = new HEXValue(new MiniNumber(0));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(0));
//        assertTrue("should be true ", hv1.div(hv2).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(20000));
//        hv2 = new HEXValue(new MiniNumber(10000));
//        res = new HEXValue(new MiniNumber(20000 / 10000));
//        //res2 = new HEXValue(new MiniNumber(10000 / 20000));
//        assertTrue("should be true ", hv1.div(hv2).isEqual(res));
//        //assertTrue("should be true ", hv2.div(hv1).isEqual(res2));
//
//        hv1 = new HEXValue(new MiniNumber(1000000));
//        hv2 = new HEXValue(new MiniNumber(1000000));
//        res = new HEXValue(new MiniNumber(1));
//        assertTrue("should be true ", hv1.div(hv2).isEqual(res));
//        assertTrue("should be true ", hv2.div(hv1).isEqual(res));
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
//        //res2 = new HEXValue(new MiniNumber(1 / Integer.MIN_VALUE)); // Not supported
//        assertTrue("should be true ", hv1.div(hv2).isEqual(res));
//        //res = new HEXValue(hv2.getNumber().div(hv1.getNumber())); // Not supported
//        //assertTrue("should be true ", res.isEqual(res2)); // Not supported
//
//        hv1 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
//        //res2 = new HEXValue(new MiniNumber(1 / Integer.MAX_VALUE)); // Not supported
//        assertTrue("should be true ", hv1.div(hv2).isEqual(res));
//        //res = new HEXValue(hv2.getNumber().div(hv1.getNumber())); // Not supported
//        //assertTrue("should be true ", res.isEqual(res2)); // Not supported
//
//        hv1 = new HEXValue(new MiniNumber(Long.MIN_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Long.MIN_VALUE));
//        //res2 = new HEXValue(new MiniNumber(1 / Long.MIN_VALUE)); // Not supported
//        //assertTrue("should be true ", hv1.div(hv2).isEqual(res)); // Invalid result
//        //res = new HEXValue(hv2.getNumber().div(hv1.getNumber())); // Not supported
//        //assertTrue("should be true ", res.isEqual(res2)); // Not supported // Not supported
//
//        hv1 = new HEXValue(new MiniNumber(Long.MAX_VALUE));
//        hv2 = new HEXValue(new MiniNumber(1));
//        res = new HEXValue(new MiniNumber(Long.MAX_VALUE));
//        //res2 = new HEXValue(new MiniNumber(1 / Long.MAX_VALUE)); // Not supported
//        assertTrue("should be true ", hv1.div(hv2).isEqual(res));
//        //res = new HEXValue(hv2.getNumber().div(hv1.getNumber())); // Not supported
//        //assertTrue("should be true ", res.isEqual(res2)); // Not supported // Not supported
    }

    @Test
    public void testComaprisons() {
        int counter = 0;
        HEXValue hv1 = new HEXValue(new MiniData());
        HEXValue hv2 = new HEXValue(new MiniData());

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    hv1 = new HEXValue(new MiniData("0"));
                    hv2 = new HEXValue(new MiniData("0"));
                    break;
                case 2:
                    hv1 = new HEXValue(new byte[]{(byte) 0x00});
                    hv2 = new HEXValue(new byte[]{(byte) 0x00});
                    break;
                case 3:
                    hv1 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
                    hv2 = new HEXValue(new MiniNumber(Integer.valueOf(0)));
                    break;
                case 4:
                    hv1 = new HEXValue(new MiniNumber(Long.valueOf(0)));
                    hv2 = new HEXValue(new MiniNumber(Long.valueOf(0)));
                    break;
                case 5:
                    hv1 = new HEXValue(new MiniNumber(BigDecimal.valueOf(0)));
                    hv2 = new HEXValue(new MiniNumber(BigDecimal.valueOf(0)));
                    break;
                case 6:
                    hv1 = new HEXValue("0");
                    hv2 = new HEXValue("0");
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertTrue("should be true ", hv1.isEqual(hv2));
            assertTrue("should be true ", hv2.isEqual(hv1));
            assertFalse("should be false ", hv1.isLess(hv2));
            assertFalse("should be false ", hv2.isLess(hv1));
            assertTrue("should be true ", hv1.isLessEqual(hv2));
            assertTrue("should be true ", hv2.isLessEqual(hv1));
            assertFalse("should be false ", hv1.isMore(hv2));
            assertFalse("should be false ", hv2.isMore(hv1));
            assertTrue("should be true ", hv1.isMoreEqual(hv2));
            assertTrue("should be true ", hv2.isMoreEqual(hv1));
        }

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    hv1 = new HEXValue(new MiniData("5"));
                    hv2 = new HEXValue(new MiniData("10"));
                    break;
                case 2:
                    hv1 = new HEXValue(new byte[]{(byte) 0x05});
                    hv2 = new HEXValue(new byte[]{(byte) 0x0A});
                    break;
                case 3:
                    hv1 = new HEXValue(new MiniNumber(Integer.valueOf(5)));
                    hv2 = new HEXValue(new MiniNumber(Integer.valueOf(10)));
                    break;
                case 4:
                    hv1 = new HEXValue(new MiniNumber(Long.valueOf(5)));
                    hv2 = new HEXValue(new MiniNumber(Long.valueOf(10)));
                    break;
                case 5:
                    hv1 = new HEXValue(new MiniNumber(BigDecimal.valueOf(5)));
                    hv2 = new HEXValue(new MiniNumber(BigDecimal.valueOf(10)));
                    break;
                case 6:
                    hv1 = new HEXValue("5");
                    hv2 = new HEXValue("A");
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertFalse("should be false ", hv1.isEqual(hv2));
            assertFalse("should be false ", hv1.isEqual(hv2));
            assertTrue("should be true ", hv1.isLess(hv2));
            assertFalse("should be false ", hv2.isLess(hv1));
            assertTrue("should be true ", hv1.isLessEqual(hv2));
            assertFalse("should be false ", hv2.isLessEqual(hv1));
            assertFalse("should be false ", hv1.isMore(hv2));
            assertTrue("should be true ", hv2.isMore(hv1));
            assertFalse("should be false ", hv1.isMoreEqual(hv2));
            assertTrue("should be true ", hv2.isMoreEqual(hv1));
        }

        //counter = 0;
        //while (true) {
        //    counter++;
        //    switch (counter) {
        //        case 1:
        //            // When HEXValue is initialized with negative value
        //            // it will add prefix 0xFF for sign, making it positive
        //            // and larger than it is. ???
        //            hv1 = new HEXValue(new MiniData(Long.toString(Integer.MIN_VALUE)));
        //            hv2 = new HEXValue(new MiniData(Long.toString(Integer.MAX_VALUE)));
        //            //break;
        //            continue;
        //        case 2:
        //            hv1 = new HEXValue(new byte[]{(byte) 0x7F, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF});
        //            hv2 = new HEXValue(new byte[]{(byte) 0x80, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00});
        //            break;
        //        case 3:
        //            hv1 = new HEXValue(new MiniNumber(Integer.MIN_VALUE));
        //            hv2 = new HEXValue(new MiniNumber(Integer.MAX_VALUE));
        //            break;
        //        case 4:
        //            hv1 = new HEXValue(new MiniNumber(Long.MIN_VALUE));
        //            hv2 = new HEXValue(new MiniNumber(Long.MAX_VALUE));
        //            break;
        //        case 5:
        //            hv1 = new HEXValue("7FFFFFFFFFFFFFFF");
        //            hv2 = new HEXValue("8000000000000000");
        //            break;
        //        default:
        //            counter = 0;
        //    }
        //    if (counter == 0) {
        //        break;
        //    }
        //
        //    assertTrue("should be true ", hv1.isLess(hv2));
        //    assertFalse("should be false ", hv2.isLess(hv1));
        //    assertTrue("should be true ", hv1.isLessEqual(hv2));
        //    assertFalse("should be false ", hv2.isLessEqual(hv1));
        //    assertFalse("should be false ", hv1.isMore(hv2));
        //    assertTrue("should be true ", hv2.isMore(hv1));
        //    assertFalse("should be false ", hv1.isMoreEqual(hv2));
        //    assertTrue("should be true ", hv2.isMoreEqual(hv1));
        //}
    }

}
