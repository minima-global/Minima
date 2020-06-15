package org.minima.tests.objects.base;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.minima.objects.base.MiniInteger;

public class MiniIntegerTests {
    
    @Test
    public void testValueZero() {
      MiniInteger i = new MiniInteger(0);
      assertNotNull("should not be null", i);
      assertTrue("should be equal to constant zero", i.isEqual(MiniInteger.ZERO));
      assertTrue("should be smaller than constant one", i.isLess(MiniInteger.ONE));
      assertTrue("should be equal to constant one when added one", i.add(MiniInteger.ONE).isEqual(MiniInteger.ONE));
      assertTrue("should be equal to constant one when added one", i.increment().isEqual(MiniInteger.ONE));
      assertTrue("should be equal to zero when multiplied by one", i.mult(MiniInteger.ONE).isEqual(MiniInteger.ZERO));
      assertTrue("should be equal to string 0", i.toString().contentEquals("0"));
    }




}