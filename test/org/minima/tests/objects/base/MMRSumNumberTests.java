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
import java.math.BigDecimal;
import java.math.BigInteger;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

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
        assertNotNull("should not be null", j.getNumber());
        assertNotNull("should not be null", j.toString());
        // assertNotNull("should not be null", i.getData());
        assertFalse("should be equal to constant zero", j.isEqual(l));
        // assertTrue("should not be null", j.getNumber() == "1");

        assertTrue("should be equal to constant one", j.add(l).isEqual(l));
        // assertTrue("should be smaller than constant one", i.isLess(MiniNumber.ONE));
        // assertTrue("should be equal to constant one when added one",
        // i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE));
        // assertTrue("should be equal to constant one when added one",
        // i.increment().isEqual(MiniNumber.ONE));
        // assertTrue("should be equal to zero when multiplied by one",
        // i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
        // assertTrue("should be equal to string 0", i.toString().contentEquals("0"));

    }

    @Test
  public void testReadFromStrea() {
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

    public void testWriteDataStreamByte() {
      try {
        MiniNumber k = new MiniNumber(1);
        MMRSumNumber l = new MMRSumNumber(k);
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);
        l.writeDataStream(dos);
        assertNotNull(l);
        System.out.println(" i is now equal to " + l);
      } catch (final IOException e) {
        System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        assertTrue(" there should be an IOException with message input too large ",
            e.getMessage().contains(new String("input too large")));
      }
    }

}