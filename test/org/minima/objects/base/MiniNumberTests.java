package org.minima.objects.base;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;
import org.minima.objects.base.MiniNumber;

public class MiniNumberTests {

    @Test
    public void testValueZero() {
        MiniNumber i = new MiniNumber(0);
        System.out.println("i value is " + i);
        assertNotNull("should not be null", i);
        assertFalse("should be equal to constant zero", i.isEqual(MiniNumber.ONE));
        System.out.println("i.isEqual(MiniNumber.ONE) i:" + i + " resolves as " + i.isEqual(MiniNumber.ONE));
        assertTrue("should be equal to constant zero", i.isEqual(MiniNumber.ZERO));
        System.out.println("i.isEqual(MiniNumber.ZERO) i:" + i + " resolves as " + i.isEqual(MiniNumber.ZERO));
        assertTrue("should be smaller than constant one", i.isLess(MiniNumber.ONE));
        System.out.println("i.isLess(MiniNumber.ONE) i:" + i + " resolves as " + i.isLess(MiniNumber.ONE));
        assertTrue("should be equal to constant one when added one", i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        System.out.println("i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        assertTrue("should be equal to constant one when added one", i.increment().isEqual(MiniNumber.ONE));
        System.out.println(
                "i.increment().isEqual(MiniNumber.ONE) i:" + i + " resolves as " + i.increment().isEqual(MiniNumber.ONE));
        assertTrue("should be equal to zero when multiplied by one", i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        System.out.println("i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO)) i:" + i + " resolves as "
                + i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        assertTrue("should be equal to string 0", i.toString().contentEquals("0"));
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
        assertNotNull("should not be null", i);
        assertNotNull("should not be null", j);
        assertNotNull("should not be null", k);
        assertNotNull("should not be null", l);
        assertNotNull("should not be null", MiniNumber.ONE);
        System.out.println("MiniNumber.ONE value is " + MiniNumber.ONE);
        assertNotNull("should not be null", MiniNumber.ONE.getAsBigInteger());
        System.out.println("MiniNumber.ONE.getAsBigInteger() value is " + MiniNumber.ONE.getAsBigInteger());
        assertNotNull("should not be null", MiniNumber.ONE.getAsInt());
        System.out.println("MiniNumber.ONE.getAsInt() value is " + MiniNumber.ONE.getAsInt());
        assertNotNull("should not be null", MiniNumber.ONE.abs());
        System.out.println("MiniNumber.ONE.abs() value is " + MiniNumber.ONE.abs());
        assertNotNull("should not be null", MiniNumber.ONE.floor());
        System.out.println("MiniNumber.ONE.floor() value is " + MiniNumber.ONE.floor());
        assertNotNull("should not be null", MiniNumber.ONE.ceil());
        System.out.println("MiniNumber.ONE.ceil() value is " + MiniNumber.ONE.ceil());
        assertNotNull("should not be null", i.add(MiniNumber.ONE).getAsLong());
        System.out.println("i.add(MiniNumber.ONE).getAsLong() value is " + i.add(MiniNumber.ONE).getAsLong());
//        assertNotNull("should not be null", i.add(MiniNumber.ONE).getAsDouble());
//        System.out.println("i.add(MiniNumber.ONE).getAsDouble() value is " + i.add(MiniNumber.ONE).getAsDouble());
//        assertNotNull("should not be null", l.divRoundDown(MiniNumber.THOUSAND));
//        System.out.println("l.divRoundDown(MiniNumber.THOUSAND) l:" + l + " value is " + l.divRoundDown(MiniNumber.THOUSAND));
        assertNotNull("should not be null", i.increment().setSignificantDigits(19));
        System.out.println("i.increment().setSignificantDigits(19) value is " + i.increment().setSignificantDigits(19));
        assertTrue("should be equal resolves true", i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE));
        System.out.println("i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE) i:" + i + " resolves as "
                + i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE));
        assertTrue("should be equal resolves true", i.pow(1).isEqual(MiniNumber.ZERO));
        System.out.println("i.pow(1).isEqual(MiniNumber.ZERO i:" + i + " resolves as " + i.pow(1).isEqual(MiniNumber.ZERO));
        assertTrue("should be less resolves true", i.add(MiniNumber.ONE).isLess(MiniNumber.TWO));
        System.out.println("i.add(MiniNumber.ONE).isLess(MiniNumber.TWO) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).isLess(MiniNumber.TWO));
        assertTrue("should be less than or equal resolves true", i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO));
        System.out.println("i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO) i:" + i + " resolves as "
                + i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO));
        assertFalse("should be less than or equal resolves false", i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE));
        System.out.println("i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE));
        assertTrue("should be greater than resolves true", i.add(MiniNumber.TWO).isMore(MiniNumber.ONE));
        System.out.println("i.add(MiniNumber.TWO).isMore(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.TWO).isMore(MiniNumber.ONE));
        assertTrue("should be equal to resolves true", i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        System.out.println("i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        assertTrue("should be return modulo 0", i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        System.out.println("i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO) i:" + i + " resolves as "
                + i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        assertTrue("should be equal resolves true", i.increment().isEqual(MiniNumber.ONE));
        System.out.println(
                "i.increment().isEqual(MiniNumber.ONE i:" + i + " resolves as " + i.increment().isEqual(MiniNumber.ONE));
        assertTrue("should be equal resolves true", i.decrement().isEqual(MiniNumber.MINUSONE));
        System.out.println("i.decrement().isEqual(MiniNumber.MINUSONE) i:" + i + " resolves as "
                + i.decrement().isEqual(MiniNumber.MINUSONE));
        assertFalse("should setSignificant digits equal to num 2 resolves false",
                i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO));
        System.out.println("i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO) i:" + i + " resolves as "
                + i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO));
        assertFalse("should be less than resolves false", i.increment().isLess(MiniNumber.ONE));
        System.out.println(
                "i.increment().isLess(MiniNumber.ONE) i:" + i + " resolves as " + i.increment().isLess(MiniNumber.ONE));
        assertFalse("should be more than resolves false", i.increment().isMore(MiniNumber.ONE));
        System.out.println(
                "i.increment().isMore(MiniNumber.ONE) i:" + i + " resolves as " + i.increment().isMore(MiniNumber.ONE));
        assertFalse("should be more than or equal resolves false", i.isMoreEqual(MiniNumber.ONE));
        System.out.println("i.isMoreEqual(MiniNumber.ONE) i:" + i + " resolves as " + i.isMoreEqual(MiniNumber.ONE));
        assertTrue("should be more than or equal resolves true", i.isMoreEqual(MiniNumber.ZERO));
        System.out.println("i.isMoreEqual(MiniNumber.ZERO i:" + i + " resolves as " + i.isMoreEqual(MiniNumber.ZERO));
        assertTrue("should be equal resolves true", MiniNumber.ONE.getAsBigDecimal().equals(m));
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
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
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
            assertTrue("should be equal to one", i.isEqual(MiniNumber.ONE));
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
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
            assertFalse("This line should not be reached ", true);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
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
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
        }

    }

}
