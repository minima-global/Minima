package org.minima.tests.database.txpowtree;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;

import org.junit.Test;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;

public class BlockTreeNodeTests {

    @Test
    public void testConstructors() {

        TxPoW txp = new TxPoW();
        txp.calculateTXPOWID();

        BlockTreeNode btn1 = new BlockTreeNode();

        BlockTreeNode btn2 = new BlockTreeNode(txp);

        BlockTreeNode btn3 = new BlockTreeNode(btn2);

        assertEquals("should be equal ", btn2.getTxPow(), btn3.getTxPow());
        assertEquals("should be equal ", btn2.getTxPowID(), btn3.getTxPowID());
        assertEquals("should be equal ", btn2.getSuperBlockLevel(), btn3.getSuperBlockLevel());
        assertEquals("should be equal ", btn2.getCurrentLevel(), btn3.getCurrentLevel());
        assertEquals("should be equal ", btn2.getRealWeight(), btn3.getRealWeight());
        assertEquals("should be equal ", btn2.getMMRSet(), btn3.getMMRSet());
        assertEquals("should be equal ", btn2.isCascade(), btn3.isCascade());
        assertEquals("should be equal ", btn2.getState(), btn3.getState());
        assertEquals("should be equal ", btn2.getWeight(), btn3.getWeight());
        assertEquals("should be equal ", btn2.getTotalWeight(), btn3.getTotalWeight());
    }

    @Test
    public void testGettersAndSetters() {

        TxPoW txp = new TxPoW();
        txp.calculateTXPOWID();

        BlockTreeNode btn = new BlockTreeNode(txp);

        BigInteger bi = btn.getTotalWeight();
        bi = bi.add(BigInteger.TEN);
        btn.addToTotalWeight(BigInteger.TEN);
        assertEquals("should be equal ", bi, btn.getTotalWeight());

        MiniNumber mn = txp.getBlockNumber();
        assertEquals("should be equal ", mn, btn.getBlockNumber());

        btn.setCurrentLevel(123);
        assertEquals("should be equal ", 123, btn.getCurrentLevel());
    }

    @Test
    public void testParentChildFunctions() {
        TxPoW txp = new TxPoW();
        txp.calculateTXPOWID();
        BlockTreeNode btn = new BlockTreeNode(txp);

        assertNull("should be null ", btn.getParent());
        assertFalse("should be null ", btn.hasChildren());
        assertEquals("should be equal ", 0, btn.getNumberChildren());
        //assertNull("should be null ", btn1.getChild(-1));
        //assertNull("should be null ", btn1.getChild(0));
        //assertNull("should be null ", btn1.getChild(5));

        TxPoW txp_p = new TxPoW();
        txp_p.calculateTXPOWID();
        BlockTreeNode btn_p = new BlockTreeNode(txp_p);

        TxPoW txp_c1 = new TxPoW();
        txp_c1.calculateTXPOWID();
        BlockTreeNode btn_c1 = new BlockTreeNode(txp_c1);

        TxPoW txp_c2 = new TxPoW();
        txp_c2.calculateTXPOWID();
        BlockTreeNode btn_c2 = new BlockTreeNode(txp_c2);

        btn.setParent(btn_p);
        btn.addChild(btn_c1);
        btn.addChild(btn_c2);
        assertEquals("should be equal ", btn_p, btn.getParent());
        assertTrue("should be true ", btn.hasChildren());
        assertEquals("should be equal ", 2, btn.getNumberChildren());
        assertEquals("should be equal ", 2, btn.getChildren().size());
        assertEquals("should be equal ", btn_c1, btn.getChild(0));
        assertEquals("should be equal ", btn_c2, btn.getChild(1));

        btn.clearParentChildren();
        assertNull("should be null ", btn.getParent());
        assertFalse("should be null ", btn.hasChildren());
        assertEquals("should be equal ", 0, btn.getNumberChildren());
        //assertNull("should be null ", btn1.getChild(-1));
        //assertNull("should be null ", btn1.getChild(0));
        //assertNull("should be null ", btn1.getChild(5));
    }

    @Test
    public void testCheckForTxpow() {

        TxPoW[] txps = {
            new TxPoW(),
            new TxPoW(),
            new TxPoW(),
            new TxPoW(),
            new TxPoW()
        };
        for (int i = 0; i < txps.length; i++) {
            txps[i].setNonce(new MiniNumber(10 + i));
            txps[i].calculateTXPOWID();
        }

        TxPoW utxp = new TxPoW();
        utxp.setNonce(new MiniNumber(123));
        utxp.calculateTXPOWID();

        TxPoW txp = new TxPoW();
        for (int i = 0; i < txps.length; i++) {
            txp.addBlockTxPOW(txps[i]);
        }
        txp.setNonce(new MiniNumber(12345));
        txp.calculateTXPOWID();
        BlockTreeNode btn = new BlockTreeNode(txp);

        assertTrue("should be true ", btn.checkForTxpow(txp.getTxPowID()));
        assertTrue("should be true ", btn.checkForTxpow(txps[0].getTxPowID()));
        assertTrue("should be true ", btn.checkForTxpow(txps[2].getTxPowID()));
        assertTrue("should be true ", btn.checkForTxpow(txps[4].getTxPowID()));
        assertFalse("should be false ", btn.checkForTxpow(utxp.getTxPowID()));

    }

    @Test
    public void testCompare() {
        TxPoW txp1 = new TxPoW();
        txp1.calculateTXPOWID();
        BlockTreeNode btn1 = new BlockTreeNode(txp1);

        TxPoW txp2 = new TxPoW();
        txp2.calculateTXPOWID();
        BlockTreeNode btn2 = new BlockTreeNode(txp2);

        assertEquals("should be equal ", btn1.getTxPowID().compare(btn1.getTxPowID()), btn1.compareTo(btn1));
        assertEquals("should be equal ", btn2.getTxPowID().compare(btn1.getTxPowID()), btn1.compareTo(btn2));
        assertEquals("should be equal ", btn1.getTxPowID().compare(btn2.getTxPowID()), btn2.compareTo(btn1));
    }

    @Test
    public void testToString() {
        TxPoW txp = new TxPoW();
        txp.calculateTXPOWID();
        BlockTreeNode btn = new BlockTreeNode(txp);
        String exp_s = "[" + btn.getCurrentLevel() + "/" + btn.getSuperBlockLevel() + "] casc:" + btn.isCascade() + " state:" + btn.getState() + " " + btn.getTxPow().toString();
        String obj_s = btn.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
