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

import org.junit.Test;
import org.minima.objects.base.MiniInteger;

public class MiniIntegerTests {
    
    @Test
    public void testValueZero() {
      final MiniInteger i = new MiniInteger(0);
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
        System.out.println(" i is now equal to " + i.toString());
        assertTrue("should be equal to one", i.isEqual(MiniInteger.ONE));
      } catch(final IOException e) {
        System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        assertTrue(" there should not be an IOException", false);
      }
    }




}