package org.minima.objects.base;

import org.junit.jupiter.api.Test;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class MiniStringTests {

    @Test
    public void testValueString() {
        MiniString i = new MiniString("MinimaTest");
        MiniString j = new MiniString(i);
        System.out.println("MiniString(\"MinimaTest\") i is now equal to " + i);
        System.out.println("MiniString(i) j is now equal to " + j);
        assertNotNull(i, "should not be null");
        System.out.println("i value is " + i);
        assertNotNull(j, "should not be null");
        System.out.println("j value is " + j);
        assertNotNull(i.toString(), "should not be null");
        System.out.println("i.toString() value is " + i.toString());
        assertNotNull(i.getData(), "should not be null");
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
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
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
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
        }
    }

}
