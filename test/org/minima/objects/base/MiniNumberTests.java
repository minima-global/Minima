package org.minima.objects.base;

import org.junit.jupiter.api.Test;

import java.io.*;
import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;

public class MiniNumberTests {

    @Test
    public void testValueZero() {
        MiniNumber i = new MiniNumber(0);
        System.out.println("i value is " + i);
        assertNotNull(i, "should not be null");
        assertFalse(i.isEqual(MiniNumber.ONE), "should be equal to constant zero");
        System.out.println("i.isEqual(MiniNumber.ONE) i:" + i + " resolves as " + i.isEqual(MiniNumber.ONE));
        assertTrue(i.isEqual(MiniNumber.ZERO), "should be equal to constant zero");
        System.out.println("i.isEqual(MiniNumber.ZERO) i:" + i + " resolves as " + i.isEqual(MiniNumber.ZERO));
        assertTrue(i.isLess(MiniNumber.ONE), "should be smaller than constant one");
        System.out.println("i.isLess(MiniNumber.ONE) i:" + i + " resolves as " + i.isLess(MiniNumber.ONE));
        assertTrue(i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE), "should be equal to constant one when added one");
        System.out.println("i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        assertTrue(i.increment().isEqual(MiniNumber.ONE), "should be equal to constant one when added one");
        System.out.println(
                "i.increment().isEqual(MiniNumber.ONE) i:" + i + " resolves as " + i.increment().isEqual(MiniNumber.ONE));
        assertTrue(i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO), "should be equal to zero when multiplied by one");
        System.out.println("i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO)) i:" + i + " resolves as "
                + i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        assertTrue(i.toString().contentEquals("0"), "should be equal to string 0");
        System.out.println("i.toString().contentEquals(\"0\") i:" + i + " resolves as " + i.toString().contentEquals("0"));

    }

    @Test
    public void testValues() {
        MiniNumber i = new MiniNumber();
        MiniNumber j = new MiniNumber(2147483649L);
        BigInteger k = new BigInteger("2147483649");
        MiniNumber l = new MiniNumber(k);
        BigDecimal m = new BigDecimal(1.0);
        System.out.println("MiniNumber() i value is " + i);
        System.out.println("MiniNumber(2147483649L) j value is " + j);
        System.out.println("BigInteger(\"2147483649\") k value is " + k);
        System.out.println("MiniNumber(k) l value is " + l);
        System.out.println("BigDecimal(1.00) m value is " + m);
        assertNotNull(i, "should not be null");
        assertNotNull(j, "should not be null");
        assertNotNull(k, "should not be null");
        assertNotNull(l, "should not be null");
        assertNotNull(MiniNumber.ONE, "should not be null");
        System.out.println("MiniNumber.ONE value is " + MiniNumber.ONE);
        assertNotNull(MiniNumber.ONE.getAsBigInteger(), "should not be null");
        System.out.println("MiniNumber.ONE.getAsBigInteger() value is " + MiniNumber.ONE.getAsBigInteger());
        assertNotNull(MiniNumber.ONE.getAsInt(), "should not be null");
        System.out.println("MiniNumber.ONE.getAsInt() value is " + MiniNumber.ONE.getAsInt());
        assertNotNull(MiniNumber.ONE.abs(), "should not be null");
        System.out.println("MiniNumber.ONE.abs() value is " + MiniNumber.ONE.abs());
        assertNotNull(MiniNumber.ONE.floor(), "should not be null");
        System.out.println("MiniNumber.ONE.floor() value is " + MiniNumber.ONE.floor());
        assertNotNull(MiniNumber.ONE.ceil(), "should not be null");
        System.out.println("MiniNumber.ONE.ceil() value is " + MiniNumber.ONE.ceil());
        assertNotNull(i.add(MiniNumber.ONE).getAsLong(), "should not be null");
        System.out.println("i.add(MiniNumber.ONE).getAsLong() value is " + i.add(MiniNumber.ONE).getAsLong());
//        assertNotNull("should not be null", i.add(MiniNumber.ONE).getAsDouble());
//        System.out.println("i.add(MiniNumber.ONE).getAsDouble() value is " + i.add(MiniNumber.ONE).getAsDouble());
//        assertNotNull("should not be null", l.divRoundDown(MiniNumber.THOUSAND));
//        System.out.println("l.divRoundDown(MiniNumber.THOUSAND) l:" + l + " value is " + l.divRoundDown(MiniNumber.THOUSAND));
        assertNotNull(i.increment().setSignificantDigits(19), "should not be null");
        System.out.println("i.increment().setSignificantDigits(19) value is " + i.increment().setSignificantDigits(19));
        assertTrue(i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE), "should be equal resolves true");
        System.out.println("i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE) i:" + i + " resolves as "
                + i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE));
        assertTrue(i.pow(1).isEqual(MiniNumber.ZERO), "should be equal resolves true");
        System.out.println("i.pow(1).isEqual(MiniNumber.ZERO i:" + i + " resolves as " + i.pow(1).isEqual(MiniNumber.ZERO));
        assertTrue(i.add(MiniNumber.ONE).isLess(MiniNumber.TWO), "should be less resolves true");
        System.out.println("i.add(MiniNumber.ONE).isLess(MiniNumber.TWO) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).isLess(MiniNumber.TWO));
        assertTrue(i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO), "should be less than or equal resolves true");
        System.out.println("i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO) i:" + i + " resolves as "
                + i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO));
        assertFalse(i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE), "should be less than or equal resolves false");
        System.out.println("i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE));
        assertTrue(i.add(MiniNumber.TWO).isMore(MiniNumber.ONE), "should be greater than resolves true");
        System.out.println("i.add(MiniNumber.TWO).isMore(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.TWO).isMore(MiniNumber.ONE));
        assertTrue(i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE), "should be equal to resolves true");
        System.out.println("i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        assertTrue(i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO), "should be return modulo 0");
        System.out.println("i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        assertTrue(i.increment().isEqual(MiniNumber.ONE), "should be equal resolves true");
        System.out.println(
                "i.increment().isEqual(MiniNumber.ONE i:" + i + " resolves as " + i.increment().isEqual(MiniNumber.ONE));
        assertTrue(i.decrement().isEqual(MiniNumber.MINUSONE), "should be equal resolves true");
        System.out.println("i.decrement().isEqual(MiniNumber.MINUSONE) i:" + i + " resolves as "
                + i.decrement().isEqual(MiniNumber.MINUSONE));
        assertFalse(i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO), "should setSignificant digits equal to num 2 resolves false");
        System.out.println("i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO) i:" + i + " resolves as "
                + i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO));
        assertFalse(i.increment().isLess(MiniNumber.ONE), "should be less than resolves false");
        System.out.println(
                "i.increment().isLess(MiniNumber.ONE) i:" + i + " resolves as " + i.increment().isLess(MiniNumber.ONE));
        assertFalse(i.increment().isMore(MiniNumber.ONE), "should be more than resolves false");
        System.out.println(
                "i.increment().isMore(MiniNumber.ONE) i:" + i + " resolves as " + i.increment().isMore(MiniNumber.ONE));
        assertFalse(i.isMoreEqual(MiniNumber.ONE), "should be more than or equal resolves false");
        System.out.println("i.isMoreEqual(MiniNumber.ONE) i:" + i + " resolves as " + i.isMoreEqual(MiniNumber.ONE));
        assertTrue(i.isMoreEqual(MiniNumber.ZERO), "should be more than or equal resolves true");
        System.out.println("i.isMoreEqual(MiniNumber.ZERO i:" + i + " resolves as " + i.isMoreEqual(MiniNumber.ZERO));
        assertTrue(MiniNumber.ONE.getAsBigDecimal().equals(m), "should be equal resolves true");
        System.out.println("MiniNumber.ONE.getAsBigDecimal().equals(m) m:" + m + " value is "
                + MiniNumber.ONE.getAsBigDecimal().equals(m));
    }

    @Test
    public void testWriteDataStream() {
        try {
            MiniNumber i = new MiniNumber("1");
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            i.writeDataStream(dos);
            assertNotNull(i);
            System.out.println(" i is now equal to " + i);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
        }
    }

    @Test
    public void testReadDataStream() {
        try {
            MiniNumber i = new MiniNumber("1");
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            MiniNumber.ONE.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            i.readDataStream(dis);
            assertNotNull(i);
            assertTrue(i.isEqual(MiniNumber.ONE), "should be equal to one");
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(false, " there should not be an IOException");
        }

    }

    @Test
    public void testReadStreamTooLarge() {
        try {
            MiniNumber i = new MiniNumber();
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            for (int j = 0; j < 1000; j++) {
                bos.write(127); // large possible value, MSB is +/- sign
            }

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            i.readDataStream(dis);
            System.out.println(" i is now equal to " + i.toString());
            assertFalse(true, "This line should not be reached ");
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
        }

    }

    @Test
    public void testReadFromStream() {
        try {
            MiniNumber i = new MiniNumber("1");
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            MiniNumber.THOUSAND.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            i.ReadFromStream(dis);
            assertNotNull(i);
            System.out.println("i.ReadFromStream(dis) i is now equal to " + i);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
        }

    }

}
