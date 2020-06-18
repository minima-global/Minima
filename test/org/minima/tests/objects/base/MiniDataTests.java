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
import org.minima.objects.base.MiniData;

public class MiniDataTests {
    @Test
    public void testValueBytes() {
    //   byte[] mData;
      MiniData i = new MiniData();
      MiniData j = new MiniData("#FFFFFF");
      MiniData k = new MiniData("5475746f7269616c73706f696e74");
      MiniData[] mData = {j,k};
      MiniData[] mDataCompare = {i,j,k};
    //   MiniData k = new MiniData(true);
    //   MiniData L = new MiniData(false);
    //   MiniData M = new MiniData(byt);
    //   assertNotNull("should not be null", i);
      assertNotNull("should not be null", i.getLength());
      assertNotNull("should not be null", i.getData());
      assertNotNull("should not be null", i.getDataValue());
      assertNotNull("should not be null", i.getDataValueDecimal());
      assertNotNull("should not be null", i.to0xString());
      assertNotNull("should not be null", i.toString());
      assertFalse("should be equal to zero", i.equals(j));
      assertTrue("should be equal to one", j.equals(j));
      assertFalse("should be equal to one", i.equals(j));
      assertFalse("should be equal to one", i.isEqual(j));
      assertTrue("should be equal to one", mData[0].isEqual(mDataCompare[1]));
      assertTrue("should be equal to one", i.isLess(j));
      assertFalse("should be equal to one", j.isLess(i));
      assertFalse("should be equal to one", j.isLessEqual(i));
      assertTrue("should be equal to one", j.isLessEqual(j));
      assertTrue("should be equal to one", j.isMore(i));
      assertFalse("should be equal to one", i.isMore(j));
      assertFalse("should be equal to one", i.isMoreEqual(j));
      assertTrue("should be equal to one", j.isMoreEqual(j));
      assertNotNull("should not be null", mData[0].compare(k));
      assertNotNull("should not be null", i.shiftr(1000));
      assertNotNull("should not be null", i.shiftl(1000));
      assertNotNull("should not be null", i.concat(j));
      assertNotNull("should not be null", i.to0xString(1));
      assertNotNull("should not be null", i.to0xString(10000));
    //   assertFalse("should be equal to constant zero", i.isEqual(j));
    //   assertTrue("should be equal to constant zero", i.isEqual(L));
    //   assertTrue("should be equal to constant zero", j.isTrue());
    //   assertFalse("should be equal to constant zero", i.isTrue());
    //   System.out.println("The bitwise value is " + j.toString());
    //   assertNotNull("should not be null", j.toString());
}

@Test
  public void testReadFromStreamData() {
    try {
        MiniData i = new MiniData();
        MiniData j = new MiniData("#FFFFFF");
        MiniData k = new MiniData("5475746f7269616c73706f696e74");
        MiniData[] mData = {j,k};
        MiniData[] mDataCompare = {i,j,k};
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      DataOutputStream dos = new DataOutputStream(bos);
      j.writeDataStream(dos);
    //   k.writeDataStream(dos);

      InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
      DataInputStream dis = new DataInputStream(inputStream);
      j.readHashFromStream(dis);  
    //   i.ReadFromStream(dis);
    //   k.ReadFromStream(dis);
      assertNotNull(j);
      System.out.println(" j is now equal to " + j);
    } catch (final IOException e) {
      System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
      assertTrue(" there should be an IOException with message input too large ",
          e.getMessage().contains(new String("input too large")));
    }

  }


@Test
public void testReadFromStreamInt() {
  try {
      MiniData i = new MiniData();
      MiniData j = new MiniData("#FFFFFF");
      MiniData k = new MiniData("5475746f7269616c73706f696e74");
      MiniData[] mData = {j,k};
      MiniData[] mDataCompare = {i,j,k};
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(bos);
    i.writeDataStream(dos);
  //   k.writeDataStream(dos);

    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
    DataInputStream dis = new DataInputStream(inputStream);
    // j.readFromStream(dis);  
    i.ReadFromStream(dis);
  //   k.ReadFromStream(dis);
    assertNotNull(j);
    System.out.println(" j is now equal to " + j);
  } catch (final IOException e) {
    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
    assertTrue(" there should be an IOException with message input too large ",
        e.getMessage().contains(new String("input too large")));
  }

}


public void testWriteDataStreamByte() {
    try {
      MiniData k = new MiniData();
    //   MMRSumNumber l = new MMRSumNumber(k);
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      DataOutputStream dos = new DataOutputStream(bos);
      k.writeDataStream(dos);
      assertNotNull(k);
      System.out.println(" k is now equal to " + k);
    } catch (final IOException e) {
      System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
      assertTrue(" there should be an IOException with message input too large ",
          e.getMessage().contains(new String("input too large")));
    }
  }

}