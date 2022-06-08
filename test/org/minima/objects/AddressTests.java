package org.minima.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;

public class AddressTests {

    @Test
    public void testAddress() {
        MiniData c = new MiniData();
        MiniData j = new MiniData("0xFFFF");
        MiniData n = new MiniData("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
        Address ad = new Address();
        Address a = new Address("0xf0f0");
        Address abc = new Address("0xf0f0");
        assertNotNull("should not be null", a);
        // System.out.println("address value " + a);
        a.getAddressData();
        // System.out.println("address value " + a.getAddressData());
        Address adr = new Address(j);
        // System.out.println("address value " + adr);
        Address adrTwo = new Address(n);
        // System.out.println("address value " + adrTwo);
        Address adrThree = new Address(c);
        // System.out.println("address value " + adrThree);
        // System.out.println("json  value " + adrTwo.toJSON());
        // System.out.println("script  value " + adrTwo.getScript());
        // System.out.println("minima addess  value " + adrTwo.getMinimaAddress());
        assertFalse("should not be equal ", a.isEqual(adrThree));
        assertTrue("should not be equal ", a.isEqual(abc));

    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            MiniData i = new MiniData("0xfff0f0");
            Address a = new Address(i);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            a.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            a.readDataStream(dis);
            // System.out.println("minima  value " + a.toString());

            assertNotNull(i);
            assertEquals("0xFFF0F0", a.toString());
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }

    @Test
    public void testMakeMinimaAddress() {

        MiniData i = new MiniData("0xffffffffffffffffffffffffffffffffffffffff");

        String mxAddress = Address.makeMinimaAddress(i);
        MiniData j = Address.convertMinimaAddress(mxAddress);
        try {
            assertEquals("should be equal", i, j);
            System.out.println("should be equal to - " + i);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        MiniData q = new MiniData(
                "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
        // byte[] data1 = q.getData();

        // //First hash it to add some checksum digits..
        // byte[] hash1 = Crypto.getInstance().hashData(data1, 160);
        // //Calculate a new length - ONLY certain lengths allowed!
        // int len1 = data1.length;
        // System.out.println("New 32bit len1 " + len1);
        String mxAddress1 = Address.makeMinimaAddress(q);
        MiniData p = Address.convertMinimaAddress(mxAddress1);
        try {
            assertEquals("should be equal", q, p);
            // System.out.println("should be equal to - " + q);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        MiniData l = new MiniData("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
        MiniData o = new MiniData("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
        MiniData v = new MiniData("0xffffffffffffffffffffffffffffffffffffffff");

        String mxAddress3 = Address.makeMinimaAddress(l);
        String mxAddress4 = Address.makeMinimaAddress(v);
        String mxAddress5 = Address.makeMinimaAddress(o);
        MiniData m = Address.convertMinimaAddress(mxAddress3);
        MiniData m2 = Address.convertMinimaAddress(mxAddress4);
        MiniData m3 = Address.convertMinimaAddress(mxAddress5);

        try {
            assertEquals("should be equal", l, m);
            assertEquals("should be equal", v, m2);
            assertEquals("should be equal", o, m3);

            // System.out.println("should be equal to - " + l);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

    }
}
