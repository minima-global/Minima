package org.minima.tests.objects.base;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import java.math.BigInteger;
import java.io.InputStream;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.nio.charset.Charset;
import org.junit.Test;
import org.minima.objects.base.MiniNumber;

public class MiniNumberTests {

  @Test
  public void testValueZero() {
    MiniNumber i = new MiniNumber(0);
    assertNotNull("should not be null", i);
    assertFalse("should be equal to constant zero", i.isEqual(MiniNumber.ONE));

    assertTrue("should be equal to constant zero", i.isEqual(MiniNumber.ZERO));
    assertTrue("should be smaller than constant one", i.isLess(MiniNumber.ONE));
    assertTrue("should be equal to constant one when added one", i.add(MiniNumber.ONE).isEqual(MiniNumber.ONE));
    assertTrue("should be equal to constant one when added one", i.increment().isEqual(MiniNumber.ONE));
    assertTrue("should be equal to zero when multiplied by one", i.mult(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
    assertTrue("should be equal to string 0", i.toString().contentEquals("0"));

  }

  @Test
  public void testValues() {
    MiniNumber i = new MiniNumber();
    MiniNumber j = new MiniNumber(2147483649L);
    BigInteger k = new BigInteger("2147483649");
    MiniNumber L = new MiniNumber(k);
    assertNotNull("should not be null", i);
    assertNotNull("should not be null", MiniNumber.ONE);
    assertNotNull("should not be null", MiniNumber.ONE.getAsBigDecimal());
    assertNotNull("should not be null", MiniNumber.ONE.getAsBigInteger());
    assertNotNull("should not be null", MiniNumber.ONE.getAsInt());
    assertNotNull("should not be null", MiniNumber.ONE.abs());
    assertNotNull("should not be null", MiniNumber.ONE.floor());
    assertNotNull("should not be null", MiniNumber.ONE.ceil());
    assertTrue("should be equal to num -1", i.sub(MiniNumber.ONE).isEqual(MiniNumber.MINUSONE));
    assertTrue("should be equal to num 1", i.pow(1).isEqual(MiniNumber.ZERO));
    assertTrue("should be equal to num 1", i.add(MiniNumber.ONE).isLess(MiniNumber.TWO));
    assertTrue("should be equal to num 1", i.add(MiniNumber.TWO).isLessEqual(MiniNumber.TWO));
    assertFalse("should be equal to num 1", i.add(MiniNumber.TWO).isLessEqual(MiniNumber.ONE));
    assertTrue("should be greater than 1", i.add(MiniNumber.TWO).isMore(MiniNumber.ONE));
    assertTrue("should be equal to 1", i.add(MiniNumber.ONE).div(MiniNumber.ONE).isEqual(MiniNumber.ONE));
    assertTrue("should be equal to 0", i.add(MiniNumber.ONE).modulo(MiniNumber.ONE).isEqual(MiniNumber.ZERO));
    assertTrue("should be equal to 1", i.increment().isEqual(MiniNumber.ONE));
    assertTrue("should be equal to -1", i.decrement().isEqual(MiniNumber.MINUSONE));
    assertNotNull("should be equal to 1L", i.add(MiniNumber.ONE).getAsLong());
    assertNotNull("should be equal to 1", i.add(MiniNumber.ONE).getAsDouble());
    assertNotNull("should be equal to Long", j);
    assertNotNull("should be equal to a BigInteger", L);
    assertNotNull("should not be null", i.divRoundDown(MiniNumber.THOUSAND));
    assertNotNull("should not be null", i.increment().setSignificantDigits(19));
    assertFalse("should be equal to num 2", i.increment().setSignificantDigits(2).isEqual(MiniNumber.TWO));
    assertFalse("should be less than num 1", i.increment().isLess(MiniNumber.ONE));
    assertFalse("should be greater than num 0", i.increment().isMore(MiniNumber.ONE));
    assertFalse("should be equal to num 0", i.isMoreEqual(MiniNumber.ONE));
    assertTrue("should be greater than num 0", i.isMoreEqual(MiniNumber.ZERO));

  }

  @Test
  public void testWriteDataStream() {
    try {
      MiniNumber i = new MiniNumber("1");
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

  @Test
  public void testReadDataStream() {
    try {
      MiniNumber i = new MiniNumber("1");
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      DataOutputStream dos = new DataOutputStream(bos);
      MiniNumber.ONE.writeDataStream(dos);

      InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
      DataInputStream dis = new DataInputStream(inputStream);

      i.readDataStream(dis);
      assertNotNull(i);

      assertTrue("should be equal to one", i.isEqual(MiniNumber.ONE));
      // dis.reset();
      // i = MiniInteger.ReadFromStream(dis);
      // assertTrue("should be equal to one", i.isEqual(MiniInteger.ONE));
    } catch (final IOException e) {
      System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
      assertTrue(" there should not be an IOException", false);
    }

  }

  @Test
  public void testReadDataStreamBigInput() {
    try {
      MiniNumber i = new MiniNumber();
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      DataOutputStream dos = new DataOutputStream(bos);
      MiniNumber.THOUSAND.writeDataStream(dos);

      InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
      DataInputStream dis = new DataInputStream(inputStream);

      i.readDataStream(dis);
      assertNotNull(i);

      // assertTrue("should be equal to one", i.isEqual(MiniNumber.ONE));
      // dis.reset();
      // i = MiniInteger.ReadFromStream(dis);
      // assertTrue("should be equal to one", i.isEqual(MiniInteger.ONE));
    } catch (final IOException e) {
      System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
      assertTrue(" there should not be an IOException", false);
    }

  }

  @Test
  public void testReadFromStream() {
    try {
      MiniNumber i = new MiniNumber("1");
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      DataOutputStream dos = new DataOutputStream(bos);
      MiniNumber.THOUSAND.writeDataStream(dos);

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

}
