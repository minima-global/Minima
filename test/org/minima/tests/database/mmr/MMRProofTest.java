package org.minima.tests.database.mmr;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import org.junit.Test;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.PubPrivKey;
import org.minima.utils.json.JSONObject;

public class MMRProofTest {

    @Test
    public void testcheckCoin() {

        PubPrivKey pk1 = new PubPrivKey(512);
        PubPrivKey pk2 = new PubPrivKey(512);
        String script1 = "RETURN SIGNEDBY ( " + pk1.getPublicKey() + " )";
        String script2 = "RETURN SIGNEDBY ( " + pk2.getPublicKey() + " )";
        Address addr1 = new Address(script1, pk1.getBitLength());
        Address addr2 = new Address(script2, pk2.getBitLength());

        Coin coin1 = new Coin(new MiniData("0x00"), addr1.getAddressData(), MiniNumber.TEN, new MiniData("0x00"));
        Coin coin2 = new Coin(new MiniData("0x01"), addr1.getAddressData(), MiniNumber.TEN, new MiniData("0x00"));
        Coin coin3 = new Coin(new MiniData("0x00"), addr2.getAddressData(), MiniNumber.TEN, new MiniData("0x00"));
        Coin coin4 = new Coin(new MiniData("0x00"), addr1.getAddressData(), MiniNumber.ONE, new MiniData("0x00"));
        Coin coin5 = new Coin(new MiniData("0x00"), addr1.getAddressData(), MiniNumber.TEN, new MiniData("0x01"));

        ArrayList<StateVariable> states = new ArrayList<StateVariable>();
        states.clear();
        states.add(new StateVariable(0, "dummy"));

        MMRData mmrd1 = new MMRData(new MiniByte(123), coin1, new MiniNumber(1234567890), states);
        MMRData mmrd2 = new MMRData(new MiniByte(123), coin2, new MiniNumber(1234567890), states);

        MMRProof mmrp1 = new MMRProof(new MiniNumber(1234567890), mmrd1, new MiniNumber(987654321));
        MMRProof mmrp2 = new MMRProof(new MiniNumber(1234567890), mmrd2, new MiniNumber(123456789));

        assertTrue("should be the same coin", mmrp1.checkCoin(coin1));
        assertFalse("should not be the same coin", mmrp1.checkCoin(coin2));
        assertFalse("should not be the same coin", mmrp1.checkCoin(coin3));
        assertFalse("should not be the same coin", mmrp1.checkCoin(coin4));
        assertFalse("should not be the same coin", mmrp1.checkCoin(coin5));

        assertFalse("should not be the same coin", mmrp2.checkCoin(coin1));
        assertTrue("should be the same coin", mmrp2.checkCoin(coin2));
        assertFalse("should not be the same coin", mmrp2.checkCoin(coin3));
        assertFalse("should not be the same coin", mmrp2.checkCoin(coin4));
        assertFalse("should not be the same coin", mmrp2.checkCoin(coin5));

    }

    @Test
    public void testWriteAndReadDataStream() {
        //{
        //    try {
        //        MMRProof mmrp1 = new MMRProof();
        //
        //        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //        DataOutputStream dos = new DataOutputStream(bos);
        //
        //        //mmrp1.writeDataStream(dos);
        //        InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //        DataInputStream dis = new DataInputStream(inputStream);
        //
        //        MMRProof mmrp2 = new MMRProof();
        //        mmrp2.readDataStream(dis);
        //
        //        assertEquals("should be equal ", mmrp1.getBlockTime().getAsBigDecimal(), mmrp2.getBlockTime().getAsBigDecimal());
        //        assertEquals("should be equal ", mmrp1.getEntryNumber().getNumber(), mmrp2.getEntryNumber().getNumber());
        //        assertEquals("should be equal ", mmrp1.getMMRData().getFinalHash(), mmrp2.getMMRData().getFinalHash());
        //
        //        InputStream inputStream2 = new ByteArrayInputStream(bos.toByteArray());
        //        DataInputStream dis2 = new DataInputStream(inputStream2);
        //
        //        MMRProof mmrp3 = MMRProof.ReadFromStream(dis2);
        //
        //        assertEquals("should be equal ", mmrp1.getBlockTime().getAsBigDecimal(), mmrp3.getBlockTime().getAsBigDecimal());
        //        assertEquals("should be equal ", mmrp1.getEntryNumber().getNumber(), mmrp3.getEntryNumber().getNumber());
        //        assertEquals("should be equal ", mmrp1.getMMRData().getFinalHash(), mmrp3.getMMRData().getFinalHash());
        //
        //    } catch (final IOException e) {
        //        System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //        assertTrue(" there should not be an IOException", false);
        //    }
        //}

        {
            try {
                MMRProof mmrp1 = new MMRProof(new MiniNumber(1234567890), new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                mmrp1.writeDataStream(dos);
                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MMRProof mmrp2 = new MMRProof();
                mmrp2.readDataStream(dis);

                assertEquals("should be equal ", mmrp1.getBlockTime().getAsBigDecimal(), mmrp2.getBlockTime().getAsBigDecimal());
                assertEquals("should be equal ", mmrp1.getEntryNumber().getNumber(), mmrp2.getEntryNumber().getNumber());
                assertEquals("should be equal ", mmrp1.getMMRData().getFinalHash(), mmrp2.getMMRData().getFinalHash());

                InputStream inputStream2 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis2 = new DataInputStream(inputStream2);

                MMRProof mmrp3 = MMRProof.ReadFromStream(dis2);

                assertEquals("should be equal ", mmrp1.getBlockTime().getAsBigDecimal(), mmrp3.getBlockTime().getAsBigDecimal());
                assertEquals("should be equal ", mmrp1.getEntryNumber().getNumber(), mmrp3.getEntryNumber().getNumber());
                assertEquals("should be equal ", mmrp1.getMMRData().getFinalHash(), mmrp3.getMMRData().getFinalHash());

            } catch (final IOException e) {
                System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
                assertTrue(" there should not be an IOException", false);
            }
        }
    }

    @Test
    public void testJSONConversion() {
        //{
        //    MMRProof mmrp = new MMRProof();
        //    JSONObject json = mmrp.toJSON();
        //    assertTrue("JSON object should contain blocktime key", json.containsKey("blocktime"));
        //    assertTrue("JSON object should contain entry key", json.containsKey("entry"));
        //    assertTrue("JSON object should contain data key", json.containsKey("data"));
        //    assertTrue("JSON object should contain proof key", json.containsKey("proof"));
        //}

        {
            MMRProof mmrp = new MMRProof(new MiniNumber(1234567890), new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));
            JSONObject json = mmrp.toJSON();
            assertTrue("JSON object should contain blocktime key", json.containsKey("blocktime"));
            assertTrue("JSON object should contain entry key", json.containsKey("entry"));
            assertTrue("JSON object should contain data key", json.containsKey("data"));
            assertTrue("JSON object should contain proof key", json.containsKey("proof"));
        }

    }

    @Test
    public void testToString() {
        //{
        //    MMRProof mmrp = new MMRProof();
        //    String exp_s = mmrp.toJSON().toString();
        //    String obj_s = mmrp.toString();
        //    assertEquals("should be equal ", exp_s, obj_s);
        //}

        {
            MMRProof mmrp = new MMRProof(new MiniNumber(1234567890), new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));
            String exp_s = mmrp.toJSON().toString();
            String obj_s = mmrp.toString();
            assertEquals("should be equal ", exp_s, obj_s);
        }
    }
}
