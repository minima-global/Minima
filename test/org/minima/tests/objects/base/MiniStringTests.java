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
      assertNotNull("should not be null", i);
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

}