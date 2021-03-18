package org.minima.tests.database.txpowdb.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.java.JavaDBRow;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class JavaDBRowTest {

    @Test
    public void testConstructors() {
        TxPoW txp = new TxPoW();
        JavaDBRow jdbr = new JavaDBRow(txp);

        assertEquals("should be equal ", txp, jdbr.getTxPOW());
        assertFalse("should be false ", jdbr.isInBlock());
        assertFalse("should be false ", jdbr.isMainChainBlock());
        assertEquals("should be equal ", TxPOWDBRow.TXPOWDBROW_STATE_BASIC, jdbr.getBlockState());
        assertEquals("should be equal ", MiniNumber.ZERO, jdbr.getInBlockNumber());
        assertFalse("should be false ", jdbr.isMonoTonic());
    }

    @Test
    public void testGettersAndSetters() {
        TxPoW txp = new TxPoW();
        JavaDBRow jdbr = new JavaDBRow(txp);

        assertEquals("should be equal ", txp, jdbr.getTxPOW());
        assertFalse("should be false ", jdbr.isInBlock());
        assertFalse("should be false ", jdbr.isMainChainBlock());
        assertEquals("should be equal ", TxPOWDBRow.TXPOWDBROW_STATE_BASIC, jdbr.getBlockState());
        assertEquals("should be equal ", "BASIC", jdbr.getStatusAsString());
        assertEquals("should be equal ", MiniNumber.ZERO, jdbr.getInBlockNumber());
        assertFalse("should be false ", jdbr.isMonoTonic());

        jdbr.setIsInBlock(true);
        jdbr.setMainChainBlock(true);
        jdbr.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
        jdbr.setInBlockNumber(MiniNumber.EIGHT);
        jdbr.setMonotonic(true);

        assertTrue("should be true ", jdbr.isInBlock());
        assertTrue("should be true ", jdbr.isMainChainBlock());
        assertEquals("should be equal ", TxPOWDBRow.TXPOWDBROW_STATE_FULL, jdbr.getBlockState());
        assertEquals("should be equal ", "FULL", jdbr.getStatusAsString());
        assertEquals("should be equal ", MiniNumber.EIGHT, jdbr.getInBlockNumber());
        assertTrue("should be true ", jdbr.isMonoTonic());

        jdbr.setBlockState(-1);
        assertEquals("should be equal ", "ERROR", jdbr.getStatusAsString());
    }

    @Test
    public void testJSONConversion() {
        JavaDBRow jdbr = new JavaDBRow(new TxPoW());

        JSONObject json = jdbr.toJSON();

        assertTrue("JSON object should contain txpow key", json.containsKey("txpow"));
        assertTrue("JSON object should contain isonchainblock key", json.containsKey("isonchainblock"));
        assertTrue("JSON object should contain isinblock key", json.containsKey("isinblock"));
        assertTrue("JSON object should contain inblock key", json.containsKey("inblock"));
        assertTrue("JSON object should contain blockstate key", json.containsKey("blockstate"));
        assertTrue("JSON object should contain monotonic key", json.containsKey("monotonic"));
        assertTrue("JSON object should contain deleted key", json.containsKey("deleted"));
    }

    @Test
    public void testToString() {
        JavaDBRow jdbr = new JavaDBRow(new TxPoW());

        String exp_s = jdbr.toJSON().toString();
        String obj_s = jdbr.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }

}
