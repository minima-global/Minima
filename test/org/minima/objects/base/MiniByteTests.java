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

import org.junit.Test;
import org.minima.objects.base.MiniByte;

public class MiniByteTests {
    @Test
    public void testValueBytes() {
        byte byt = 0;
        MiniByte i = new MiniByte();
        MiniByte j = new MiniByte(1);
        MiniByte k = new MiniByte(true);
        MiniByte l = new MiniByte(false);
        MiniByte m = new MiniByte(byt);
        System.out.println("byte byt value is " + byt);
        System.out.println("MiniByte() i value is " + i);
        System.out.println("MiniByte(1) j value is " + j);
        System.out.println("MiniByte(true) k value is " + k);
        System.out.println("MiniByte(false) l value is " + l);
        System.out.println("MiniByte(byt) m value is " + m);
        assertNotNull("should not be null", i);
        System.out.println("i:" + i + " resolves as " + i);
        assertNotNull("should not be null", k.getByteValue());
        System.out.println("k.getByteValue() k:" + k + " resolves as " + k);
        assertFalse("should be equal resolves false", i.isEqual(j));
        System.out.println("i.isEqual(j) i:" + i + " j:" + j + " resolves as " + i.equals(j));
        assertTrue("should be equal resolves true", i.isEqual(l));
        System.out.println("i.isEqual(l) i:" + i + " l:" + l + " resolves as " + i.isEqual(l));
        assertTrue("should be true resolves true", j.isTrue());
        System.out.println("j.isTrue() j:" + j + " resolves as " + j.isTrue());
        assertFalse("should be false resolves false", i.isTrue());
        System.out.println("i.isTrue() i:" + i + " resolves as " + i.isTrue());
        assertNotNull("should not be null", j.toString());
        System.out.println("j.toString() The bitwise value is " + j.toString());
    }

    @Test
    public void testReadFromByte() {
        try {
            MiniByte i = new MiniByte(0);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            i.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            i.ReadFromStream(dis);
            assertNotNull(i);
            System.out.println(" i is now equal to " + i);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
        }

    }

    public void testWriteDataStreamByte() {
        try {
            MiniByte i = new MiniByte(0);
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

}
