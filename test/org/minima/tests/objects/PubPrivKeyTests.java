package org.minima.tests.objects;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.PubPrivKey;
import org.minima.utils.json.JSONObject;

public class PubPrivKeyTests {

    public static final int w = 12;

    @Test
    public void testConstructors() {
        PubPrivKey k = new PubPrivKey();
        MiniData emptySeed = k.getPrivateSeed();
        assertTrue("default seed should be empty", emptySeed == null);
        k = new PubPrivKey(w * 3 * 8);
        MiniData nonEmptySeed = k.getPrivateSeed();
        assertTrue("seed should exist", nonEmptySeed != null);
        assertTrue("seed should have correct length", nonEmptySeed.getData().length == w * 3);
        assertTrue("key bitlength has correct length", k.getBitLength() == 3 * w * 8);
        byte[] pubkey = k.getPublicKey().getData();
        assertTrue("pub key should have correct length", pubkey.length == 3 * w);
        String pubkeyStr = k.toString();
        assertNotNull("key str should not be null", pubkeyStr);
        JSONObject json = k.toJSON();
        System.out.println("JSON: " + json.toJSONString());
        assertTrue("JSON object should contain public key", json.containsValue(pubkeyStr));
        assertTrue("JSON object should have correct length", json.containsValue(3 * w * 8));

        byte[] seedBytes = new byte[w * 3];
        for (byte i = 0; i < seedBytes.length; i++) {
            seedBytes[i] = i;
        }
        MiniData theSeed = new MiniData(seedBytes);
        k = new PubPrivKey(theSeed);
        MiniData nonEmptySeed2 = k.getPrivateSeed();
        assertTrue("seed should exist", nonEmptySeed2 != null);
        assertTrue("seed should have correct length", nonEmptySeed2.getData().length == w*3);
        
    }

    private void testSigningAndVerifying(String msg) {
        PubPrivKey k = new PubPrivKey(w * 3 * 8);
        MiniData miniMsg = new MiniData(msg.getBytes());
        MiniData sig = k.sign(miniMsg);
        assertNotNull("signature should not be null");
        assertFalse("signature hould not have value zero", sig.isEqual(new MiniData("0")));
        assertTrue("signature is valid", k.verify(miniMsg, sig));
    }

    @Test
    public void testSignAndVerifyHelloWorld() {
        testSigningAndVerifying("Hello World");
    }

    public void testSignAndVerifyLoremIpsum() {
        testSigningAndVerifying("Lorem ipsum");
    }

    public void testSignAndVerifyGenesis() {
        testSigningAndVerifying("The Times 03/Jan/2009 Chancellor on brink of second bailout for banks");
    }

    public void testSignAndVerifyEvolution() {
        testSigningAndVerifying("The Evolution will not be Centralised");
    }
}
