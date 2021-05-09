package org.minima.tests.kissvm.values;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;

import org.junit.Test;
import org.minima.kissvm.values.HexValue;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class HexValueTests {

    @Test
    public void testConstructors() {

        HexValue hv1 = new HexValue(new MiniData());
        HexValue hv2 = new HexValue(new MiniData("1234"));

        assertThrows(NumberFormatException.class, () -> {
            new HexValue(new MiniData("QWER")); //invalid HEX input
        });
        assertThrows(NumberFormatException.class, () -> {
            new HexValue(new MiniNumber(-1)); // Invalid HEX input
        });
        assertThrows(NumberFormatException.class, () -> {
            new HexValue(new MiniNumber("1.1")); // Invalid HEX input
        });

        HexValue hv4 = new HexValue(new MiniData(new byte[]{}));
        HexValue hv5 = new HexValue(new MiniData(new byte[]{(byte) 0x12, (byte) 0x34, (byte) 0x56}));
        HexValue hv6 = new HexValue(new MiniNumber(Integer.valueOf(255)));
        HexValue hv7 = new HexValue(new MiniNumber(Long.valueOf(65535)));
        HexValue hv8 = new HexValue(new MiniNumber(BigDecimal.valueOf(16777215)));
        HexValue hv9 = new HexValue(new MiniNumber("1.0"));

        assertEquals("should be equal ", HexValue.VALUE_HEX, hv1.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv2.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv4.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv5.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv6.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv7.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv8.getValueType());
        assertEquals("should be equal ", HexValue.VALUE_HEX, hv9.getValueType());
    }

    @Test
    public void testComaprisons() {
        int counter = 0;
        HexValue hv1 = new HexValue(new MiniData());
        HexValue hv2 = new HexValue(new MiniData());

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    hv1 = new HexValue(new MiniData("0"));
                    hv2 = new HexValue(new MiniData("0"));
                    break;
                case 2:
                    hv1 = new HexValue(new byte[]{(byte) 0x00});
                    hv2 = new HexValue(new byte[]{(byte) 0x00});
                    break;
                case 3:
                    hv1 = new HexValue(new MiniNumber(Integer.valueOf(0)));
                    hv2 = new HexValue(new MiniNumber(Integer.valueOf(0)));
                    break;
                case 4:
                    hv1 = new HexValue(new MiniNumber(Long.valueOf(0)));
                    hv2 = new HexValue(new MiniNumber(Long.valueOf(0)));
                    break;
                case 5:
                    hv1 = new HexValue(new MiniNumber(BigDecimal.valueOf(0)));
                    hv2 = new HexValue(new MiniNumber(BigDecimal.valueOf(0)));
                    break;
                case 6:
                    hv1 = new HexValue("0");
                    hv2 = new HexValue("0");
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertTrue("should be true ", hv1.isEqual(hv2));
            assertTrue("should be true ", hv2.isEqual(hv1));
            
//            assertFalse("should be false ", hv1.isLess(hv2));
//            assertFalse("should be false ", hv2.isLess(hv1));
//            assertTrue("should be true ", hv1.isLessEqual(hv2));
//            assertTrue("should be true ", hv2.isLessEqual(hv1));
//            assertFalse("should be false ", hv1.isMore(hv2));
//            assertFalse("should be false ", hv2.isMore(hv1));
//            assertTrue("should be true ", hv1.isMoreEqual(hv2));
//            assertTrue("should be true ", hv2.isMoreEqual(hv1));
        }

        counter = 0;
        while (true) {
            counter++;
            switch (counter) {
                case 1:
                    hv1 = new HexValue(new MiniData("5"));
                    hv2 = new HexValue(new MiniData("10"));
                    break;
                case 2:
                    hv1 = new HexValue(new byte[]{(byte) 0x05});
                    hv2 = new HexValue(new byte[]{(byte) 0x0A});
                    break;
                case 3:
                    hv1 = new HexValue(new MiniNumber(Integer.valueOf(5)));
                    hv2 = new HexValue(new MiniNumber(Integer.valueOf(10)));
                    break;
                case 4:
                    hv1 = new HexValue(new MiniNumber(Long.valueOf(5)));
                    hv2 = new HexValue(new MiniNumber(Long.valueOf(10)));
                    break;
                case 5:
                    hv1 = new HexValue(new MiniNumber(BigDecimal.valueOf(5)));
                    hv2 = new HexValue(new MiniNumber(BigDecimal.valueOf(10)));
                    break;
                case 6:
                    hv1 = new HexValue("5");
                    hv2 = new HexValue("A");
                    break;
                default:
                    counter = 0;
            }
            if (counter == 0) {
                break;
            }

            assertFalse("should be false ", hv1.isEqual(hv2));
            assertFalse("should be false ", hv1.isEqual(hv2));
           
//            assertTrue("should be true ", hv1.isLess(hv2));
//            assertFalse("should be false ", hv2.isLess(hv1));
//            assertTrue("should be true ", hv1.isLessEqual(hv2));
//            assertFalse("should be false ", hv2.isLessEqual(hv1));
//            assertFalse("should be false ", hv1.isMore(hv2));
//            assertTrue("should be true ", hv2.isMore(hv1));
//            assertFalse("should be false ", hv1.isMoreEqual(hv2));
//            assertTrue("should be true ", hv2.isMoreEqual(hv1));
        }
    }

}
