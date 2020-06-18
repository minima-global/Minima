package org.minima.tests.objects.base;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.nio.charset.Charset;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

import org.checkerframework.checker.units.qual.m;
import org.junit.Test;
import org.minima.objects.base.MiniString;

public class MiniStringTests {
    

    @Test
    public void testValueString() {
        MiniString i = new MiniString("MinimaTest");
        MiniString j = new MiniString(i);
      assertNotNull("should not be null", i);
      assertNotNull("should not be null", j);
      assertNotNull("should not be null", i.toString());
      assertNotNull("should not be null", i.getData());
    //   assertNotNull("should not be null", i.getData());
    //   assertFalse("should be equal to constant zero", i.isEqual(MiniNumber.ONE));

    //   assertTrue("should be equal to constant zero", i.isEqual(MiniNumber.ZERO));
    //   assertTrue("should be smaller than constant one", i.isLess(MiniNumber.ONE));
    //   assertTrue("should be equal to constant one when added one", i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE));
    //   assertTrue("should be equal to constant one when added one", i.increment().isEqual(MiniNumber.ONE));
    //   assertTrue("should be equal to zero when multiplied by one", i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
    //   assertTrue("should be equal to string 0", i.toString().contentEquals("0"));

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