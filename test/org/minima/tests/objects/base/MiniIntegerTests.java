package org.minima.tests.objects.base;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.base.MiniInteger;

public class MiniIntegerTests {

    @Test
    public void testValueZero() {
        MiniInteger i = new MiniInteger(0);
        assertNotNull("should not be null", i);
        assertTrue("should be equal to constant zero", i.isEqual(MiniInteger.ZERO));
        assertFalse("should not be equal to constant one", i.isEqual(MiniInteger.ONE));
        assertTrue("should be smaller than constant one", i.isLess(MiniInteger.ONE));
        assertFalse("should be larger than constant minus one", i.isLess(new MiniInteger(-1)));
        assertTrue("should be equal to constant one when added one", i.add(MiniInteger.ONE).isEqual(MiniInteger.ONE));
        assertTrue("should be equal to constant one when added one", i.increment().isEqual(MiniInteger.ONE));
        assertTrue("should be equal to zero when multiplied by one", i.mult(MiniInteger.ONE).isEqual(MiniInteger.ZERO));
        assertTrue("should be equal to string 0", i.toString().contentEquals("0"));
        assertTrue(" 0 modulo 0 should return 0", i.modulo(MiniInteger.ONE).isEqual(MiniInteger.ZERO));
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            MiniInteger.ONE.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            i.readDataStream(dis);
            assertTrue("should be equal to one", i.isEqual(MiniInteger.ONE));
            dis.reset();
            i = MiniInteger.ReadFromStream(dis);
            assertTrue("should be equal to one", i.isEqual(MiniInteger.ONE));
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
        i = new MiniInteger("0");
        assertTrue(" read string 0 return mini integer 0", i.isEqual(MiniInteger.ZERO));
        MiniInteger minus1 = i.sub(MiniInteger.ONE);
        assertTrue(" 0 - 1 should equal -1", minus1.isEqual(new MiniInteger(-1)));

    }

    @Test
    public void testReadStreamTooLarge() {
        try {
            MiniInteger i = new MiniInteger(0);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            //MiniInteger.ONE.writeDataStream(dos);

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
    public void testDivRoundDown() {
        MiniInteger three = new MiniInteger(3);
        MiniInteger result = three.divRoundDown(MiniInteger.TWO);
        assertTrue(" 3 / 2 should be equal to 1", result.isEqual(MiniInteger.ONE));
        result = MiniInteger.TWO.divRoundDown(MiniInteger.TWO);
        assertTrue(" 2 / 2 should be equal to 1", result.isEqual(MiniInteger.ONE));
        result = MiniInteger.TWO.divRoundDown(MiniInteger.ONE);
        assertTrue(" 2 / 1 should be equal to 2", result.isEqual(MiniInteger.TWO));
        result = MiniInteger.ONE.divRoundDown(MiniInteger.TWO);
        assertTrue(" 1 / 2 should be equal to 0", result.isEqual(MiniInteger.ZERO));

    }
}
