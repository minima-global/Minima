package org.minima.tests.objects;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class TxPoWTests {

    @Test
    public void testTxPoWTests() {
        TxPoW txpow = new TxPoW();
        txpow.setBlockDifficulty(new MiniData("0xffff"));
        assertTrue("New TxPow should have a tx header", txpow.getTxHeader() != null);
        assertTrue("New TxPow should have a tx body", txpow.getTxBody() != null);
        assertTrue("New TxPow should have a tx body", txpow.hasBody());
        assertTrue("New TxPow should not have a bodyhash", txpow.getTxHeaderBodyHash().isEqual(new MiniData("0x0")));
        assertTrue("New TxPow should not be a transaction", txpow.isTransaction() == false);
        assertTrue("New TxPow should return empty block txs", txpow.getBlockTransactions().isEmpty());
        assertTrue("New TxPow should have empty super parent", txpow.getNonce().isEqual(new MiniNumber(0)));
        assertTrue("New TxPow should not be a block", txpow.isBlock() == false);
        assertTrue("New TxPow should have super level zero", txpow.getSuperLevel() == 0);
        assertTrue("New TxPow should be on main chain", txpow.getChainID().isEqual(new MiniData("0x00")));
        assertTrue("New TxPow parent chain id should be zero", txpow.getParentChainID().isEqual(new MiniData("0x00")));
        assertTrue("New TxPow should have txpowid zero", txpow.getTxPowID().isEqual(new MiniData("0x00")));
        assertTrue("New TxPow should have transid zero", txpow.getTransID().isEqual(new MiniData("0x00")));
        assertTrue("New TxPow should have a witness", txpow.getWitness() != null);
        txpow.calculateTXPOWID();
        assertTrue("New TxPow computedtxpowid should not be zero", txpow.getTxPowID() != new MiniData("0x00"));

        JSONObject json = txpow.toJSON();
        assertTrue("JSON object is not null", json != null);

        try {

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            txpow.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            TxPoW txpowRead = new TxPoW();
            txpowRead.readDataStream(dis);
            assertTrue("Read/Write stream should retun an object with non nul txpowid", txpow.getTxPowID() != new MiniData("0x00"));

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }
}
