package org.minima.tests.database.txpowdb.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.java.FastJavaDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class FastJavaDBTest {

    @Test
    public void testConstructors() {
        FastJavaDB fjdb = new FastJavaDB();
    }

    @Test
    public void testTxPowOperations() {
        FastJavaDB jdb = new FastJavaDB();

        TxPoW txp0 = new TxPoW();
        txp0.setNonce(new MiniNumber(10));
        txp0.setBlockNumber(new MiniNumber(100));
        txp0.calculateTXPOWID();
        TxPoW txp1 = new TxPoW();
        txp1.setNonce(new MiniNumber(11));
        txp1.setBlockNumber(new MiniNumber(101));
        txp1.calculateTXPOWID();
        TxPoW txp2 = new TxPoW();
        txp2.setNonce(new MiniNumber(12));
        txp2.setBlockNumber(new MiniNumber(102));
        txp2.calculateTXPOWID();
        TxPoW txp3 = new TxPoW();
        txp3.setNonce(new MiniNumber(13));
        txp3.setBlockNumber(new MiniNumber(103));
        txp3.calculateTXPOWID();
        TxPoW txp4 = new TxPoW();
        txp4.setNonce(new MiniNumber(14));
        txp4.setBlockNumber(new MiniNumber(104));
        txp4.calculateTXPOWID();
        TxPoW txp5 = new TxPoW();
        txp5.setNonce(new MiniNumber(15));
        txp5.setBlockNumber(new MiniNumber(105));
        txp5.calculateTXPOWID();
        TxPoW txp6 = new TxPoW();
        txp6.setNonce(new MiniNumber(16));
        txp6.setBlockNumber(new MiniNumber(106));
        txp6.calculateTXPOWID();
        TxPoW txp7 = new TxPoW();
        txp7.setNonce(new MiniNumber(17));
        txp7.setBlockNumber(new MiniNumber(107));
        txp7.calculateTXPOWID();
        TxPoW txp8 = new TxPoW();
        txp8.setNonce(new MiniNumber(18));
        txp8.setBlockNumber(new MiniNumber(108));
        txp8.calculateTXPOWID();

        jdb.addTxPOWDBRow(txp1);
        jdb.addTxPOWDBRow(txp2);
        jdb.addTxPOWDBRow(txp1);
        jdb.addTxPOWDBRow(txp2);
        jdb.addTxPOWDBRow(txp3);
        jdb.addTxPOWDBRow(txp4);
        jdb.addTxPOWDBRow(txp5);
        jdb.addTxPOWDBRow(txp6);
        jdb.addTxPOWDBRow(txp7);
        jdb.addTxPOWDBRow(txp8);
        assertEquals("should be equal ", 8, jdb.getSize());
        assertEquals("should be equal ", 8, jdb.getAllTxPOWDBRow().size());

        jdb.removeTxPOW(txp1.getTxPowID());
        assertEquals("should be equal ", 7, jdb.getSize());
        assertEquals("should be equal ", 7, jdb.getAllTxPOWDBRow().size());

        jdb.removeTxPOW(txp1.getTxPowID());
        assertEquals("should be equal ", 7, jdb.getSize());
        assertEquals("should be equal ", 7, jdb.getAllTxPOWDBRow().size());

        TxPOWDBRow txp0a = jdb.findTxPOWDBRow(txp0.getTxPowID());
        assertNull("should be null ", txp0a);

        TxPOWDBRow txp1r = jdb.findTxPOWDBRow(txp1.getTxPowID());
        assertNull("should be null ", txp1r);
        //assertEquals("should be equal ", txp1.getTxPowID(), txp1r.getTxPOW().getTxPowID());

        TxPOWDBRow txp2r = jdb.findTxPOWDBRow(txp2.getTxPowID());
        assertEquals("should be equal ", txp2.getTxPowID(), txp2r.getTxPOW().getTxPowID());

        //jdb.findTxPOWDBRow(txp1.getTxPowID()).setIsInBlock(true);
        jdb.findTxPOWDBRow(txp2.getTxPowID()).setIsInBlock(true);
        jdb.findTxPOWDBRow(txp3.getTxPowID()).setIsInBlock(true);
        jdb.findTxPOWDBRow(txp4.getTxPowID()).setIsInBlock(true);
        ArrayList<TxPOWDBRow> altxp = jdb.getAllUnusedTxPOW();
        assertEquals("should be equal ", 4, altxp.size());

        jdb.findTxPOWDBRow(txp5.getTxPowID()).setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
        ArrayList<TxPOWDBRow> altxpu = jdb.getAllBlocksMissingTransactions();
        assertEquals("should be equal ", 6, altxpu.size());

        ArrayList<TxPOWDBRow> altxpcb = jdb.getChildBlocksTxPOW(new MiniData());
        assertEquals("should be equal ", 8, altxpcb.size());

        jdb.resetBlocksFromOnwards(new MiniNumber(102));

        jdb.resetAllInBlocks();

        ArrayList<TxPOWDBRow> deleted = jdb.removeTxPOWInBlockLessThan(new MiniNumber(94));
        assertEquals("should be equal ", 0, deleted.size());

        jdb.findTxPOWDBRow(txp5.getTxPowID()).setMainChainBlock(true);
        jdb.findTxPOWDBRow(txp6.getTxPowID()).setIsInBlock(true);
        ArrayList<TxPOWDBRow> deleted1 = jdb.removeTxPOWInBlockLessThan(new MiniNumber(97));
        assertEquals("should be equal ", 0, deleted1.size());

        assertNotEquals("should not be equal ", 0, jdb.getSize());
        jdb.ClearDB();
        assertEquals("should be equal ", 0, jdb.getSize());

    }
}
