package org.minima.objects.base;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.base.MiniString;

public class MiniStringTests {

    @Test
    public void testValueString() {
        MiniString i = new MiniString("MinimaTest");
        MiniString j = new MiniString(i);
        System.out.println("MiniString(\"MinimaTest\") i is now equal to " + i);
        System.out.println("MiniString(i) j is now equal to " + j);
        assertNotNull("should not be null", i);
        System.out.println("i value is " + i);
        assertNotNull("should not be null", j);
        System.out.println("j value is " + j);
        assertNotNull("should not be null", i.toString());
        System.out.println("i.toString() value is " + i.toString());
        assertNotNull("should not be null", i.getData());
        System.out.println("i.getData() value is " + i.getData());
    }

    @Test
    public void testReadFromStream() {
        try {
            MiniString i = new MiniString("test");
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

    public void testWriteDataStreamString() {
        try {
            MiniString i = new MiniString("t");
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
