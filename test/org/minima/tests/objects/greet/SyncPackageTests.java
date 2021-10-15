package org.minima.tests.objects.greet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
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
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.SyncPacket;
import org.minima.utils.MiniFile;

public class SyncPackageTests {

    @Test
    public void testConstructors() {
        {
            SyncPackage sp = new SyncPackage();

            assertEquals(0, sp.getAllNodes().size());
            assertEquals(MiniNumber.ZERO, sp.getCascadeNode());
            assertEquals(BigInteger.ZERO.longValue(), sp.calculateWeight().longValue());

            sp.setCascadeNode(MiniNumber.ONE);
            assertEquals(MiniNumber.ONE, sp.getCascadeNode());
            sp.setCascadeNode(MiniNumber.TWO);
            assertEquals(MiniNumber.TWO, sp.getCascadeNode());
            sp.setCascadeNode(MiniNumber.FOUR);
            assertEquals(MiniNumber.FOUR, sp.getCascadeNode());
        }
    }

    @Test
    public void testGettersAndSetters() {
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            SyncPackage sp = new SyncPackage();

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            sp.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            SyncPackage sp1 = new SyncPackage();
            sp1.readDataStream(dis);

            assertEquals(sp.getAllNodes().size(), sp1.getAllNodes().size());
            assertEquals(sp.getCascadeNode().getAsInt(), sp1.getCascadeNode().getAsInt());
            assertEquals(sp.calculateWeight().longValue(), sp1.calculateWeight().longValue());
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testToString() {
        {
            SyncPackage sp = new SyncPackage();

            String exp_s = "";
            for (SyncPacket node : sp.getAllNodes()) {
                exp_s += node + ",";
            }
            String obj_s = sp.toString();
            assertEquals(exp_s, obj_s);
        }
    }
}
