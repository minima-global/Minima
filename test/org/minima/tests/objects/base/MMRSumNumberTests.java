package org.minima.tests.objects.base;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.io.InputStream;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;

import org.junit.Test;
import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniNumber;

public class MMRSumNumberTests {

    @Test
    public void testValueString() {
        MiniNumber i = new MiniNumber(0);
        MMRSumNumber j = new MMRSumNumber(i);
        MiniNumber k = new MiniNumber(1);
        MMRSumNumber l = new MMRSumNumber(k);
        System.out.println("MiniNumber(0) i value is " + i);
        System.out.println("MMRSumNumber(i) j value is " + j);
        System.out.println("MiniNumber(1) k value is " + k);
        System.out.println("MMRSumNumber(k) l value is " + l);
        assertNotNull("should not be null", j.getNumber());
        System.out.println("j.getNumber() resolves as " + j.getNumber());
        assertNotNull("should not be null", j.toString());
        System.out.println("j.toString() resolves as " + j.toString());
        assertFalse("should be equal resolves false", j.isEqual(l));
        System.out.println("j.isEqual(l) resolves as " + j.isEqual(l));
        assertTrue("should be equal resolves true", j.add(l).isEqual(l));
        System.out.println("j.add(l).isEqual(l) resolves as " + j.add(l).isEqual(l));
    }

    @Test
    public void testReadFromStream() {
        try {
            MiniNumber k = new MiniNumber(1);
            MMRSumNumber l = new MMRSumNumber(k);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            l.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            l.ReadFromStream(dis);
            assertNotNull(l);
            System.out.println(" i is now equal to " + l);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
        }

    }

    @Test
    public void testReadStreamTooLarge() {
        try {
            MiniNumber k = new MiniNumber(1);
            MMRSumNumber i = new MMRSumNumber(k);
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

    public void testWriteDataStreamByte() {
        try {
            MiniNumber k = new MiniNumber(1);
            MMRSumNumber l = new MMRSumNumber(k);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            l.writeDataStream(dos);
            assertNotNull(l);
            System.out.println(" i is now equal to " + l);
            assertFalse("This line should not be reached ", true);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
        }
    }

}