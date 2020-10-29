package org.minima.tests.database.mmr;

import org.minima.database.mmr.MMRSet;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;

import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.base.*;

import org.minima.utils.json.JSONObject;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import org.junit.Test;

public class MMRSetTest {

    @Test
    public void testWriteAndReadDataStream() {
        //try {
        //    MMRSet mmrs1 = new MMRSet();
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    mmrs1.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    MMRSet mmrs2 = new MMRSet();
        //    mmrs2.readDataStream(dis);
        //
        //    assertEquals("should be equal ", mmrs1., mmrs2.;
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}

        //try {
        //    MMRSet mmrs1 = new MMRSet(512);
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    mmrs1.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    MMRSet mmrs2 = new MMRSet();
        //    mmrs2.readDataStream(dis);
        //
        //    //assertEquals("should be equal ", mmrs1., mmrs2.;
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}

        //try {
        //    MMRSet mmrs1 = new MMRSet(512);
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    mmrs1.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    MMRSet mmrs2 = new MMRSet();
        //    mmrs2.readDataStream(dis);
        //
        //    //assertEquals("should be equal ", mmrs1., mmrs2.;
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}

        try {
            MMRSet mmrs1 = new MMRSet();

            MMRProof mmrp1 = new MMRProof(new MiniInteger(1234567890), new MMRData(new MiniData("0x01"), new MMRSumNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));
            mmrs1.addExternalUnspentCoin(mmrp1);
            MMRProof mmrp2 = new MMRProof(new MiniInteger(123456789), new MMRData(new MiniData("0x03"), new MMRSumNumber(new MiniNumber(123456789))), new MiniNumber(98765432));
            mmrs1.addExternalUnspentCoin(mmrp2);
            //MMRProof mmrp3 = new MMRProof(new MiniInteger(1234567891), new MMRData(new MiniData("0x02"), new MMRSumNumber(new MiniNumber(1234567891))), new MiniNumber(987654322));
            //mmrs1.addExternalUnspentCoin(mmrp3);

            MMRSet mmrs2 = new MMRSet(mmrs1);
            //mmrs2.setParent(mmrs1);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            mmrs2.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            MMRSet mmrs3 = new MMRSet();
            mmrs3.readDataStream(dis);

            //assertEquals("should be equal ", mmrs1., mmrs2.;
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }


        try {
            MMRSet mmrs1 = new MMRSet();

            MMRData mmrd1 = new MMRData(new MiniData(), new MMRSumNumber(new MiniNumber(1234567890)));

            PubPrivKey pk = new PubPrivKey(512);
            String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
            Address addr = new Address(script, pk.getBitLength());
            ArrayList<StateVariable> states = new ArrayList<StateVariable>();
            states.clear();
            states.add(new StateVariable(0, "dummy"));

            MMRData mmrd2 = new MMRData(new MiniByte(0), new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00")), new MiniNumber(1234567890), states);

            mmrs1.addUnspentCoin(mmrd1);
            mmrs1.addUnspentCoin(mmrd2);

            MMRProof mmrp1 = new MMRProof(new MiniInteger(1234567890), new MMRData(new MiniData("0x01"), new MMRSumNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));
            mmrs1.addExternalUnspentCoin(mmrp1);
            MMRProof mmrp2 = new MMRProof(new MiniInteger(123456789), new MMRData(new MiniData("0x03"), new MMRSumNumber(new MiniNumber(123456789))), new MiniNumber(98765432));
            mmrs1.addExternalUnspentCoin(mmrp2);

            MMRSet mmrs2 = new MMRSet(mmrs1);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            mmrs2.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            MMRSet mmrs3 = new MMRSet();
            mmrs3.readDataStream(dis);

            //assertEquals("should be equal ", mmrs1., mmrs2.;
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }

    @Test
    public void testJSONConversion() {
        {
            MMRSet mmrs = new MMRSet();
            JSONObject json = mmrs.toJSON();
            assertTrue("JSON object should contain block key", json.containsKey("block"));
            assertTrue("JSON object should contain entrynumber key", json.containsKey("entrynumber"));
            assertTrue("JSON object should contain entries key", json.containsKey("entries"));
            assertTrue("JSON object should contain maxrow key", json.containsKey("maxrow"));
            assertTrue("JSON object should contain maxentries key", json.containsKey("maxentries"));
            assertTrue("JSON object should contain keepers key", json.containsKey("keepers"));
        }

        {
            MMRSet mmrs = new MMRSet(512);
            JSONObject json = mmrs.toJSON();
            assertTrue("JSON object should contain block key", json.containsKey("block"));
            assertTrue("JSON object should contain entrynumber key", json.containsKey("entrynumber"));
            assertTrue("JSON object should contain entries key", json.containsKey("entries"));
            assertTrue("JSON object should contain maxrow key", json.containsKey("maxrow"));
            assertTrue("JSON object should contain maxentries key", json.containsKey("maxentries"));
            assertTrue("JSON object should contain keepers key", json.containsKey("keepers"));
        }

    }
}
