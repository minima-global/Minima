package org.minima.tests.database.mmr;

import org.minima.database.mmr.MMREntry;

import org.minima.database.mmr.MMRData;

import org.minima.objects.base.MiniInteger;
import org.minima.utils.json.JSONObject;

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
import java.math.BigInteger;
import java.util.ArrayList;

import org.junit.Test;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class MMREntryTest {

    @Test
    public void testNavigation() {
        {
            MMREntry mmre1 = new MMREntry(123, new MiniInteger(100));
            MMREntry mmre2 = new MMREntry(1234, new MiniInteger(201));
            MMREntry mmre3 = new MMREntry(123, new MiniInteger(101));
            MMREntry mmre4 = new MMREntry(124, new MiniInteger(100));

            assertTrue("should be true ", mmre1.checkPosition(123, new MiniInteger(100)));
            assertTrue("should be true ", mmre1.checkPosition(mmre1));

            assertFalse("should be false ", mmre1.checkPosition(123, new MiniInteger(101)));
            assertFalse("should be false ", mmre1.checkPosition(124, new MiniInteger(100)));
            assertFalse("should be false ", mmre1.checkPosition(1234, new MiniInteger(101)));
            assertFalse("should be false ", mmre1.checkPosition(mmre2));
            assertFalse("should be false ", mmre1.checkPosition(mmre3));
            assertFalse("should be false ", mmre1.checkPosition(mmre4));

            assertEquals("should be equal ", 123, mmre1.getRow());
            assertEquals("should be equal ", 124, mmre1.getParentRow());
            assertEquals("should be equal ", 122, mmre1.getChildRow());
            assertTrue("should be left ", mmre1.isLeft());
            assertFalse("should not be right ", mmre1.isRight());
            assertEquals("should be equal ", new MiniInteger(99).getNumber(), mmre1.getLeftSibling().getNumber());
            assertEquals("should be equal ", new MiniInteger(101).getNumber(), mmre1.getRightSibling().getNumber());
            assertEquals("should be equal ", new MiniInteger(101).getNumber(), mmre1.getSibling().getNumber()); // Is this correct (if left, return right and vice versa)
            assertEquals("should be equal ", new MiniInteger(50).getNumber(), mmre1.getParentEntry().getNumber());
            assertEquals("should be equal ", new MiniInteger(200).getNumber(), mmre1.getLeftChildEntry().getNumber());
            assertEquals("should be equal ", new MiniInteger(201).getNumber(), mmre1.getRightChildEntry().getNumber());

            assertEquals("should be equal ", 1234, mmre2.getRow());
            assertEquals("should be equal ", 1235, mmre2.getParentRow());
            assertEquals("should be equal ", 1233, mmre2.getChildRow());
            assertFalse("should not be left ", mmre2.isLeft());
            assertTrue("should be right ", mmre2.isRight());
            assertEquals("should be equal ", new MiniInteger(200).getNumber(), mmre2.getLeftSibling().getNumber());
            assertEquals("should be equal ", new MiniInteger(202).getNumber(), mmre2.getRightSibling().getNumber());
            assertEquals("should be equal ", new MiniInteger(200).getNumber(), mmre2.getSibling().getNumber()); // Is this correct (if left, return right and vice versa)
            assertEquals("should be equal ", new MiniInteger(100).getNumber(), mmre2.getParentEntry().getNumber());
            assertEquals("should be equal ", new MiniInteger(402).getNumber(), mmre2.getLeftChildEntry().getNumber());
            assertEquals("should be equal ", new MiniInteger(403).getNumber(), mmre2.getRightChildEntry().getNumber());

        }

    }

    @Test
    public void testCompare() {
        MMREntry mmre1 = new MMREntry(123, new MiniInteger(100));
        MMREntry mmre2 = new MMREntry(1234, new MiniInteger(201));

        assertEquals("should be equal ", 0, mmre1.compareTo(mmre1));
        //assertTrue("should be lower", mmre1.compareTo(mmre2) < 0);
        //assertTrue("should be higher ", mmre2.compareTo(mmre1) > 0);
    }

    @Test
    public void testWriteAndReadDataStream() {
        //{
        //    try {
        //        MMREntry mmre1 = new MMREntry(123, new MiniInteger(1234567890));
        //
        //        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //        DataOutputStream dos = new DataOutputStream(bos);
        //
        //        mmre1.writeDataStream(dos);
        //
        //        InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //        DataInputStream dis = new DataInputStream(inputStream);
        //
        //        MMREntry mmre2 = new MMREntry(0, new MiniInteger(0));
        //        mmre2.readDataStream(dis);
        //
        //        assertEquals("should be equal ", mmre1.getBlockTime().getAsBigDecimal(), mmre2.getBlockTime().getAsBigDecimal());
        //        assertEquals("should be equal ", mmre1.getChildRow(), mmre2.getChildRow());
        //        assertEquals("should be equal ", mmre1.getEntryNumber().getNumber(), mmre2.getEntryNumber().getNumber());
        //        assertEquals("should be equal ", 0, mmre1.getHashValue().compare(mmre2.getHashValue()));
        //        assertEquals("should be equal ", mmre1.getParentRow(), mmre2.getParentRow());
        //        assertEquals("should be equal ", mmre1.getRow(), mmre2.getRow());
        //    } catch (final IOException e) {
        //        System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //        assertTrue(" there should not be an IOException", false);
        //    }
        //}

        {
            try {
                MMREntry mmre1 = new MMREntry(123, new MiniInteger(1234567890));

                PubPrivKey pk = new PubPrivKey(512);
                String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
                Address addr = new Address(script, pk.getBitLength());

                Coin coin = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00"));

                ArrayList<StateVariable> states = new ArrayList<StateVariable>();
                states.clear();
                states.add(new StateVariable(0, "dummy"));

                MMRData mmrd = new MMRData(new MiniByte(123), coin, new MiniNumber(1234567890), states);
                mmre1.setData(mmrd);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                mmre1.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MMREntry mmre2 = new MMREntry(0, new MiniInteger(0));
                mmre2.readDataStream(dis);

                assertEquals("should be equal ", mmre1.getBlockTime().getAsBigDecimal(), mmre2.getBlockTime().getAsBigDecimal());
                assertEquals("should be equal ", mmre1.getChildRow(), mmre2.getChildRow());
                assertEquals("should be equal ", mmre1.getEntryNumber().getNumber(), mmre2.getEntryNumber().getNumber());
                assertEquals("should be equal ", 0, mmre1.getHashValue().compare(mmre2.getHashValue()));
                assertEquals("should be equal ", mmre1.getParentRow(), mmre2.getParentRow());
                assertEquals("should be equal ", mmre1.getRow(), mmre2.getRow());
            } catch (final IOException e) {
                System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
                assertTrue(" there should not be an IOException", false);
            }
        }
    }

    @Test
    public void testJSONConversion() {
        //{
        //    MMREntry mmre = new MMREntry(123, new MiniInteger(1234567890));
        //    JSONObject json = mmre.toJSON();
        //    assertTrue("JSON object should contain block key", json.containsKey("block"));
        //    assertTrue("JSON object should contain row key", json.containsKey("row"));
        //    assertTrue("JSON object should contain entry key", json.containsKey("entry"));
        //    assertTrue("JSON object should contain data key", json.containsKey("data"));
        //}

        {
            MMREntry mmre = new MMREntry(123, new MiniInteger(1234567890));

            PubPrivKey pk = new PubPrivKey(512);
            String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
            Address addr = new Address(script, pk.getBitLength());

            Coin coin = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00"));

            ArrayList<StateVariable> states = new ArrayList<StateVariable>();
            states.clear();
            states.add(new StateVariable(0, "dummy"));

            MMRData mmrd = new MMRData(new MiniByte(123), coin, new MiniNumber(1234567890), states);
            mmre.setData(mmrd);

            JSONObject json = mmre.toJSON();
            assertTrue("JSON object should contain block key", json.containsKey("block"));
            assertTrue("JSON object should contain row key", json.containsKey("row"));
            assertTrue("JSON object should contain entry key", json.containsKey("entry"));
            assertTrue("JSON object should contain data key", json.containsKey("data"));
        }
    }

    @Test
    public void testToString() {
        MMREntry mmre = new MMREntry(123, new MiniInteger(1234567890));
        String exp_s = "BLKTIME:" + mmre.getBlockTime() + " R:" + mmre.getRow() + " E:" + mmre.getEntryNumber() + " D:" + mmre.getData();
        String obj_s = mmre.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
