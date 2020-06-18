package org.minima.tests.objects.base;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.minima.objects.base.MiniByte;

public class MiniByteTests {
    @Test
  public void testValueBytes() {
    byte byt = 0;
    MiniByte i = new MiniByte();
    MiniByte j = new MiniByte(1);
    MiniByte k = new MiniByte(true);
    MiniByte L = new MiniByte(false);
    MiniByte M = new MiniByte(byt);
    assertNotNull("should not be null", i);
    assertNotNull("should not be null", k.getByteValue());
    assertFalse("should be equal to constant zero", i.isEqual(j));
    assertTrue("should be equal to constant zero", i.isEqual(L));
    assertTrue("should be equal to constant zero", j.isTrue());
    assertFalse("should be equal to constant zero", i.isTrue());
    System.out.println("The bitwise value is " + j.toString());
    assertNotNull("should not be null", j.toString());

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