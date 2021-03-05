package org.minima.tests.database.coin.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.database.coindb.java.JavaCoinDBRow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class JavaCoinDBRowTests {

    @Test
    public void testConstructors() {
        PubPrivKey pk = new PubPrivKey(512);
        String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
        Address addr = new Address(script, pk.getBitLength());
        Coin c1 = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.ZERO, new MiniData("0x00"));
        Coin c2 = new Coin(new MiniData("0x01"), addr.getAddressData(), MiniNumber.ONE, new MiniData("0x01"));

        JavaCoinDBRow r1 = new JavaCoinDBRow(c1);
        assertEquals("should be equal ", c1, r1.getCoin());

        JavaCoinDBRow r2 = new JavaCoinDBRow(c2);
        assertEquals("should be equal ", c2, r2.getCoin());
    }

    @Test
    public void testGettersAndSetters() {
        PubPrivKey pk = new PubPrivKey(512);
        String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
        Address addr = new Address(script, pk.getBitLength());
        Coin c1 = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.ZERO, new MiniData("0x00"));
        Coin c2 = new Coin(new MiniData("0x01"), addr.getAddressData(), MiniNumber.ONE, new MiniData("0x01"));

        JavaCoinDBRow r1 = new JavaCoinDBRow(c1);

        r1.setIsSpent(true);
        assertTrue("should be true ", r1.isSpent());
        r1.setIsSpent(false);
        assertFalse("should be false ", r1.isSpent());

        r1.setInBlockNumber(MiniNumber.ZERO);
        assertEquals("should be equal ", 0, r1.getInBlockNumber().getAsInt());
        r1.setInBlockNumber(MiniNumber.MILLION);
        assertEquals("should be equal ", 1000000, r1.getInBlockNumber().getAsInt());

        r1.setIsInBlock(true);
        assertTrue("should be true ", r1.isInBlock());
        r1.setIsInBlock(false);
        assertFalse("should be false ", r1.isInBlock());

        r1.setMMREntry(MiniNumber.ZERO);
        assertEquals("should be equal ", MiniNumber.ZERO, r1.getMMREntry());
        r1.setMMREntry(MiniNumber.TWO);
        assertEquals("should be equal ", MiniNumber.TWO, r1.getMMREntry());

        r1.setRelevant(true);
        assertTrue("should be true ", r1.isRelevant());
        r1.setRelevant(false);
        assertFalse("should be false ", r1.isRelevant());

        r1.setKeeper(true);
        assertTrue("should be true ", r1.isKeeper());
        r1.setKeeper(false);
        assertFalse("should be false ", r1.isKeeper());
    }

    @Test
    public void testJSONConversion() {
        PubPrivKey pk = new PubPrivKey(512);
        String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
        Address addr = new Address(script, pk.getBitLength());
        Coin c1 = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.ZERO, new MiniData("0x00"));

        JavaCoinDBRow r1 = new JavaCoinDBRow(c1);

        JSONObject json = r1.toJSON();

        assertTrue("JSON object should contain mmrentry key", json.containsKey("mmrentry"));
        assertTrue("JSON object should contain spent key", json.containsKey("spent"));
        assertTrue("JSON object should contain relevant key", json.containsKey("relevant"));
        assertTrue("JSON object should contain keeper key", json.containsKey("keeper"));
        assertTrue("JSON object should contain isinblock key", json.containsKey("isinblock"));
        assertTrue("JSON object should contain inblock key", json.containsKey("inblock"));
        assertTrue("JSON object should contain coin key", json.containsKey("coin"));
    }

    @Test
    public void testToString() {
        PubPrivKey pk = new PubPrivKey(512);
        String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
        Address addr = new Address(script, pk.getBitLength());
        Coin c1 = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.ZERO, new MiniData("0x00"));

        JavaCoinDBRow r1 = new JavaCoinDBRow(c1);

        String exp_s = r1.toJSON().toString();
        String obj_s = r1.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
