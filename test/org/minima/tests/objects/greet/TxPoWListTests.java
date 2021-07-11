package org.minima.tests.objects.greet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.util.ArrayList;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import org.minima.GlobalParams;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.TxPoWList;

public class TxPoWListTests {

    @Test
    public void testConstructors() {
        TxPoWList txpl = new TxPoWList();
        assertEquals(0, txpl.getList().size());
        assertEquals(0, txpl.size());
    }

    @Test
    public void testAdd() {
        TxPoWList txpl = new TxPoWList();
        assertEquals(0, txpl.getList().size());
        assertEquals(0, txpl.size());

        for (int i = 0; i < 16; i++) {
            TxPoW txp = new TxPoW();
            txp.setBlockNumber(new MiniNumber(i + 5));
            txp.calculateTXPOWID();
            txpl.addTxPow(txp);
        }
        assertEquals(16, txpl.getList().size());
        assertEquals(16, txpl.size());
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            TxPoWList txpl = new TxPoWList();
            assertEquals(0, txpl.getList().size());
            assertEquals(0, txpl.size());

            for (int i = 0; i < 16; i++) {
                TxPoW txp = new TxPoW();
                txp.setBlockNumber(new MiniNumber(i));
                txp.calculateTXPOWID();
                txpl.addTxPow(txp);
            }
            assertEquals(16, txpl.getList().size());
            assertEquals(16, txpl.size());

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            txpl.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            TxPoWList txpl1 = new TxPoWList();
            txpl1.readDataStream(dis);

            assertEquals(txpl.getList().size(), txpl.getList().size());
            assertEquals(txpl.size(), txpl.size());
        } catch (Exception e) {
            fail();
        }
    }
}
