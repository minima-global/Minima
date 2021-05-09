package org.minima.tests.database.mmr;

import static org.junit.Assert.assertEquals;
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
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.PubPrivKey;
import org.minima.utils.json.JSONObject;

public class MMRDataTest {

    @Test
    public void testWriteAndReadDataStream() {
        try {
            {
                MMRData mmrd1 = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                mmrd1.writeDataStream(dos);

                InputStream inputStream1 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis1 = new DataInputStream(inputStream1);

                MMRData mmrd2 = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));
                mmrd2.readDataStream(dis1);

                assertEquals("should be equal ", mmrd1.getFinalHash(), mmrd2.getFinalHash());
                assertEquals("should be equal ", mmrd1.getValueSum().getNumber(), mmrd2.getValueSum().getNumber());
                //assertEquals("should be equal ", mmrd1.isSpent(), mmrd2.isSpent());
                //assertEquals("should be equal ", mmrd1.getCoin(), mmrd2.getCoin());
                //assertEquals("should be equal ", mmrd1.getPrevState(), mmrd2.getPrevState());
                //assertEquals("should be equal ", mmrd1.getInBlock().getAsBigDecimal(), mmrd2.getInBlock().getAsBigDecimal());
                assertEquals("should be equal ", mmrd1.isHashOnly(), mmrd2.isHashOnly());

                InputStream inputStream2 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis2 = new DataInputStream(inputStream2);

                MMRData mmrd3 = MMRData.ReadFromStream(dis2);

                assertEquals("should be equal ", mmrd1.getFinalHash(), mmrd3.getFinalHash());
                assertEquals("should be equal ", mmrd1.getValueSum().getNumber(), mmrd3.getValueSum().getNumber());
                //assertEquals("should be equal ", mmrd1.isSpent(), mmrd3.isSpent());
                //assertEquals("should be equal ", mmrd1.getCoin(), mmrd3.getCoin());
                //assertEquals("should be equal ", mmrd1.getPrevState(), mmrd3.getPrevState());
                //assertEquals("should be equal ", mmrd1.getInBlock().getAsBigDecimal(), mmrd3.getInBlock().getAsBigDecimal());
                assertEquals("should be equal ", mmrd1.isHashOnly(), mmrd3.isHashOnly());
            }

            {
                PubPrivKey pk = new PubPrivKey(512);
                String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
                Address addr = new Address(script, pk.getBitLength());
                ArrayList<StateVariable> states = new ArrayList<StateVariable>();
                states.clear();
                states.add(new StateVariable(0, "dummy"));

                MMRData mmrd1 = new MMRData(new MiniByte(0), new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00")), new MiniNumber(1234567890), states);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                mmrd1.writeDataStream(dos);

                InputStream inputStream1 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis1 = new DataInputStream(inputStream1);

                MMRData mmrd2 = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));
                mmrd2.readDataStream(dis1);

                assertEquals("should be equal ", mmrd1.getFinalHash(), mmrd2.getFinalHash());
                assertEquals("should be equal ", mmrd1.getValueSum().getNumber(), mmrd2.getValueSum().getNumber());
                assertEquals("should be equal ", mmrd1.isSpent(), mmrd2.isSpent());
                //assertEquals("should be equal ", mmrd1.getCoin(), mmrd2.getCoin());
                //assertEquals("should be equal ", mmrd1.getPrevState(), mmrd2.getPrevState());
                assertEquals("should be equal ", mmrd1.getInBlock().getAsBigDecimal(), mmrd2.getInBlock().getAsBigDecimal());
                assertEquals("should be equal ", mmrd1.isHashOnly(), mmrd2.isHashOnly());

                InputStream inputStream2 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis2 = new DataInputStream(inputStream2);

                MMRData mmrd3 = MMRData.ReadFromStream(dis2);

                assertEquals("should be equal ", mmrd1.getFinalHash(), mmrd3.getFinalHash());
                assertEquals("should be equal ", mmrd1.getValueSum().getNumber(), mmrd3.getValueSum().getNumber());
                assertEquals("should be equal ", mmrd1.isSpent(), mmrd3.isSpent());
                //assertEquals("should be equal ", mmrd1.getCoin(), mmrd3.getCoin());
                //assertEquals("should be equal ", mmrd1.getPrevState(), mmrd3.getPrevState());
                assertEquals("should be equal ", mmrd1.getInBlock().getAsBigDecimal(), mmrd3.getInBlock().getAsBigDecimal());
                assertEquals("should be equal ", mmrd1.isHashOnly(), mmrd3.isHashOnly());
            }

            {
                PubPrivKey pk = new PubPrivKey(512);
                String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
                Address addr = new Address(script, pk.getBitLength());
                ArrayList<StateVariable> states = new ArrayList<StateVariable>();
                states.clear();
                states.add(new StateVariable(0, "dummy"));

                MMRData mmrd1 = new MMRData(new MiniByte(123), new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00")), new MiniNumber(1234567890), states);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                mmrd1.writeDataStream(dos);

                InputStream inputStream1 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis1 = new DataInputStream(inputStream1);

                MMRData mmrd2 = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));
                mmrd2.readDataStream(dis1);

                assertEquals("should be equal ", mmrd1.getFinalHash(), mmrd2.getFinalHash());
                assertEquals("should be equal ", mmrd1.getValueSum().getNumber(), mmrd2.getValueSum().getNumber());
                assertEquals("should be equal ", mmrd1.isSpent(), mmrd2.isSpent());
                //assertEquals("should be equal ", mmrd1.getCoin(), mmrd2.getCoin());
                //assertEquals("should be equal ", mmrd1.getPrevState(), mmrd2.getPrevState());
                assertEquals("should be equal ", mmrd1.getInBlock().getAsBigDecimal(), mmrd2.getInBlock().getAsBigDecimal());
                assertEquals("should be equal ", mmrd1.isHashOnly(), mmrd2.isHashOnly());

                InputStream inputStream2 = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis2 = new DataInputStream(inputStream2);

                MMRData mmrd3 = MMRData.ReadFromStream(dis2);

                assertEquals("should be equal ", mmrd1.getFinalHash(), mmrd3.getFinalHash());
                assertEquals("should be equal ", mmrd1.getValueSum().getNumber(), mmrd3.getValueSum().getNumber());
                assertEquals("should be equal ", mmrd1.isSpent(), mmrd3.isSpent());
                //assertEquals("should be equal ", mmrd1.getCoin(), mmrd3.getCoin());
                //assertEquals("should be equal ", mmrd1.getPrevState(), mmrd3.getPrevState());
                assertEquals("should be equal ", mmrd1.getInBlock().getAsBigDecimal(), mmrd3.getInBlock().getAsBigDecimal());
                assertEquals("should be equal ", mmrd1.isHashOnly(), mmrd3.isHashOnly());
            }

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    @Test
    public void testJSONConversion() {
        {
            MMRData mmrd = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));
            JSONObject json = mmrd.toJSON();
            assertTrue("JSON object should contain hashonly key", json.containsKey("hashonly"));
            assertTrue("JSON object should contain value key", json.containsKey("value"));
            assertTrue("JSON object should contain finalhash key", json.containsKey("finalhash"));
            assertTrue("JSON object should contain spent key", !json.containsKey("spent"));
            assertTrue("JSON object should contain coin key", !json.containsKey("coin"));
            assertTrue("JSON object should contain inblock key", !json.containsKey("inblock"));
            assertTrue("JSON object should contain prevstate key", !json.containsKey("prevstate"));
        }

        {
            PubPrivKey pk = new PubPrivKey(512);
            String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
            Address addr = new Address(script, pk.getBitLength());
            ArrayList<StateVariable> states = new ArrayList<StateVariable>();
            states.clear();
            states.add(new StateVariable(0, "dummy"));

            MMRData mmrd = new MMRData(new MiniByte(0), new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00")), new MiniNumber(1234567890), states);
            JSONObject json = mmrd.toJSON();
            assertTrue("JSON object should contain hashonly key", json.containsKey("hashonly"));
            assertTrue("JSON object should contain value key", json.containsKey("value"));
            assertTrue("JSON object should contain finalhash key", json.containsKey("finalhash"));
            assertTrue("JSON object should contain spent key", json.containsKey("spent"));
            assertTrue("JSON object should contain coin key", json.containsKey("coin"));
            assertTrue("JSON object should contain inblock key", json.containsKey("inblock"));
            assertTrue("JSON object should contain prevstate key", json.containsKey("prevstate"));
        }

        {
            PubPrivKey pk = new PubPrivKey(512);
            String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
            Address addr = new Address(script, pk.getBitLength());
            ArrayList<StateVariable> states = new ArrayList<StateVariable>();
            states.clear();
            states.add(new StateVariable(0, "dummy"));

            MMRData mmrd = new MMRData(new MiniByte(123), new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00")), new MiniNumber(1234567890), states);
            JSONObject json = mmrd.toJSON();
            assertTrue("JSON object should contain hashonly key", json.containsKey("hashonly"));
            assertTrue("JSON object should contain value key", json.containsKey("value"));
            assertTrue("JSON object should contain finalhash key", json.containsKey("finalhash"));
            assertTrue("JSON object should contain spent key", json.containsKey("spent"));
            assertTrue("JSON object should contain coin key", json.containsKey("coin"));
            assertTrue("JSON object should contain inblock key", json.containsKey("inblock"));
            assertTrue("JSON object should contain prevstate key", json.containsKey("prevstate"));
        }

    }

    @Test
    public void testToString() {
        MMRData mmrd = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));
        String exp_s = mmrd.toJSON().toString();
        String obj_s = mmrd.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
