package org.minima.tests.objects.greet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import org.minima.GlobalParams;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.SyncPacket;
import org.minima.utils.MiniFile;

public class SyncPacketTests {

    @Test
    public void testConstructors() {
        {
            SyncPacket sp = new SyncPacket();
            assertNull(sp.getMMRSet());
            assertNull(sp.getTxPOW());
            assertFalse(sp.isCascade());
        }
        {
            TxPoW txp = new TxPoW();
            txp.calculateTXPOWID();

            BlockTreeNode btn = new BlockTreeNode(txp);

            SyncPacket sp = new SyncPacket(btn);

            assertEquals(btn.getMMRSet(), sp.getMMRSet());
            assertEquals(txp, sp.getTxPOW());
            assertEquals(btn.isCascade(), sp.isCascade());
        }
        {
            TxPoW txp = new TxPoW();
            txp.calculateTXPOWID();

            BlockTreeNode btn = new BlockTreeNode(txp);

            SyncPacket sp = new SyncPacket(btn, true);

            assertNull(sp.getMMRSet());
            assertEquals(txp, sp.getTxPOW());
            assertEquals(btn.isCascade(), sp.isCascade());
        }
    }

    @Test
    public void testWriteAndReadDataStream() {
        // Fails on unpopulated object
        //try {
        //    SyncPacket sp = new SyncPacket();
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    sp.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    SyncPacket sp1 = new SyncPacket();
        //    sp1.readDataStream(dis);
        //
        //    assertEquals(sp.getMMRSet(), sp1.getMMRSet());
        //    assertEquals(sp.getTxPOW(), sp1.getTxPOW());
        //    assertEquals(sp.isCascade(), sp1.isCascade());
        //} catch (Exception e) {
        //    fail();
        //}

        // Fails on partially populated object
        //try {
        //    TxPoW txp = new TxPoW();
        //    txp.calculateTXPOWID();
        //
        //    BlockTreeNode btn = new BlockTreeNode(txp);
        //
        //    SyncPacket sp = new SyncPacket(btn);
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    sp.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    SyncPacket sp1 = new SyncPacket();
        //    // Read fails on partially populated object
        //    sp1.readDataStream(dis);
        //
        //    assertEquals(sp.getMMRSet(), sp1.getMMRSet());
        //    assertEquals(sp.getTxPOW(), sp1.getTxPOW());
        //    assertEquals(sp.isCascade(), sp1.isCascade());
        //} catch (Exception e) {
        //    fail();
        //}
        try {
            TxPoW txp = new TxPoW();
            txp.calculateTXPOWID();

            MMRSet mmrs = new MMRSet(512);
            mmrs.addLeafNode(MiniData.getRandomData(16));

            BlockTreeNode btn = new BlockTreeNode(txp);
            btn.setMMRset(mmrs);

            SyncPacket sp = new SyncPacket(btn);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            sp.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            SyncPacket sp1 = new SyncPacket();
            sp1.readDataStream(dis);

            //assertEquals(sp.getMMRSet(), sp1.getMMRSet());
            assertEquals(sp.getTxPOW().toString(), sp1.getTxPOW().toString());
            assertEquals(sp.isCascade(), sp1.isCascade());
        } catch (Exception e) {
            fail();
        }

        try {
            TxPoW txp = new TxPoW();
            txp.calculateTXPOWID();

            BlockTreeNode btn = new BlockTreeNode(txp);
            btn.setCascade(true);

            SyncPacket sp = new SyncPacket(btn, true);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            sp.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            SyncPacket sp1 = SyncPacket.ReadFromStream(dis);

            assertEquals(sp.getMMRSet(), sp1.getMMRSet());
            assertEquals(sp.getTxPOW().toString(), sp1.getTxPOW().toString());
            assertEquals(sp.isCascade(), sp1.isCascade());
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testToString() {
        {
            TxPoW txp = new TxPoW();
            txp.calculateTXPOWID();

            BlockTreeNode btn = new BlockTreeNode(txp);
            btn.setCascade(false);

            SyncPacket sp = new SyncPacket(btn);

            String exp_s = "MMR:" + !sp.isCascade() + " " + sp.getTxPOW();
            String obj_s = sp.toString();
            assertEquals(exp_s, obj_s);
        }
        {
            TxPoW txp = new TxPoW();
            txp.calculateTXPOWID();

            BlockTreeNode btn = new BlockTreeNode(txp);
            btn.setCascade(true);

            SyncPacket sp = new SyncPacket(btn);

            String exp_s = "MMR:" + !sp.isCascade() + " " + sp.getTxPOW();
            String obj_s = sp.toString();
            assertEquals(exp_s, obj_s);
        }
    }

    @Test
    public void testLoadBlock() {
        TxPoW txp = new TxPoW();
        txp.calculateTXPOWID();

        MMRSet mmrs = new MMRSet(512);
        mmrs.addLeafNode(MiniData.getRandomData(16));

        BlockTreeNode btn = new BlockTreeNode(txp);
        btn.setMMRset(mmrs);

        SyncPacket sp = new SyncPacket(btn);

        try {
            File f = new File("testLoadBlock.bin");
            f.createNewFile();
            MiniFile.writeObjectToFile(f, sp);
        } catch (IOException ex) {
            fail();
        }

        File f = new File("testLoadBlock.bin");
        SyncPacket sp1 = SyncPacket.loadBlock(f);

        //assertEquals(sp.getMMRSet(), sp1.getMMRSet());
        assertEquals(sp.getTxPOW().toString(), sp1.getTxPOW().toString());
        assertEquals(sp.isCascade(), sp1.isCascade());
    }

}
