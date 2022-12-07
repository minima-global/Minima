package org.minima.database.txpowdb.ram;

import org.junit.jupiter.api.Test;
import org.minima.objects.TxPoW;
import org.minima.utils.TestUtils;

import static org.junit.jupiter.api.Assertions.*;

public class RamDBTest {

    @Test
    public void testAddTxPoW() {
        RamDB txpdb = new RamDB();

        int size = txpdb.getSize();
        assertEquals(0, size);

        txpdb.addTxPoW(TestUtils.getTxPoW("0x00"));
        size = txpdb.getSize();
        assertEquals(1, size);

        txpdb.addTxPoW(TestUtils.getTxPoW("0x00"));
        size = txpdb.getSize();
        assertEquals(1, size);

        txpdb.addTxPoW(TestUtils.getTxPoW("0x00"));
        size = txpdb.getSize();
        assertEquals(1, size);

        txpdb.addTxPoW(TestUtils.getTxPoW("0x01"));
        size = txpdb.getSize();
        assertEquals(2, size);

        txpdb.addTxPoW(TestUtils.getTxPoW("0x02"));
        size = txpdb.getSize();
        assertEquals(3, size);
    }

    @Test
    public void testFindTxPoW() {
        RamDB txpdb = new RamDB();

        //Add some data
        txpdb.addTxPoW(TestUtils.getTxPoW("0x00"));
        txpdb.addTxPoW(TestUtils.getTxPoW("0x01"));
        txpdb.addTxPoW(TestUtils.getTxPoW("0x02"));

        TxPoW txp = txpdb.getTxPoW("0x00");
        assertNotNull(txp);

        txp = txpdb.getTxPoW("0x01");
        assertNotNull(txp);

        txp = txpdb.getTxPoW("0x02");
        assertNotNull(txp);

        txp = txpdb.getTxPoW("0x10");
        assertNull(txp);
    }

    @Test
    public void testCleanDB() {

    }

}
