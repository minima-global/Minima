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
import org.minima.objects.greet.Greeting;

public class GreetingTests {

    @Test
    public void testConstructors() {
        Greeting g = new Greeting();
        assertEquals(-1, g.getTopBlock().getAsInt());
        assertEquals(-1, g.getFirstBlock().getAsInt());
        assertEquals(0, g.getList().size());
        assertEquals(GlobalParams.MINIMA_VERSION, g.getVersion());
        assertEquals(0, g.getDetails().size());
    }

    @Test
    public void testGettersAndSetters() {
        Greeting g = new Greeting();
        assertEquals(-1, g.getTopBlock().getAsInt());
        assertEquals(-1, g.getFirstBlock().getAsInt());
        assertEquals(0, g.getList().size());
        assertEquals(GlobalParams.MINIMA_VERSION, g.getVersion());
        assertEquals(0, g.getDetails().size());

        for (int i = 0; i < 16; i++) {
            TxPoW txp = new TxPoW();
            txp.setBlockNumber(new MiniNumber(i + 5));
            txp.calculateTXPOWID();
            g.addBlock(txp);
        }
        assertEquals(16, g.getList().size());
        assertEquals(20, g.getTopBlock().getAsInt());
        assertEquals(5, g.getFirstBlock().getAsInt());

        {
            TxPoW txp = new TxPoW();
            txp.setBlockNumber(new MiniNumber(4));
            txp.calculateTXPOWID();
            g.addBlock(txp);
        }
        assertEquals(17, g.getList().size());
        assertEquals(20, g.getTopBlock().getAsInt());
        assertEquals(4, g.getFirstBlock().getAsInt());

        g.setTopBlock(MiniNumber.MILLION);
        assertEquals(MiniNumber.MILLION.getAsInt(), g.getTopBlock().getAsInt());

        g.setTopBlock(new MiniNumber(-1000)); // can set negative values
        assertEquals(-1000, g.getTopBlock().getAsInt());

        g.setTopBlock(new MiniNumber(-1)); // and revert to default state
        assertEquals(-1, g.getTopBlock().getAsInt());
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            Greeting g = new Greeting();
            for (int i = 0; i < 16; i++) {
                TxPoW txp = new TxPoW();
                txp.setBlockNumber(new MiniNumber(i));
                txp.calculateTXPOWID();
                g.addBlock(txp);
            }

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            g.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            Greeting g1 = Greeting.ReadFromStream(dis);

            assertEquals(g.getTopBlock().getAsInt(), g1.getTopBlock().getAsInt());
            assertEquals(g.getFirstBlock().getAsInt(), g1.getFirstBlock().getAsInt());
            assertEquals(g.getList().size(), g1.getList().size());
            assertEquals(g.getVersion(), g1.getVersion());
            assertEquals(g.getDetails().size(), g1.getDetails().size());
        } catch (Exception e) {
            fail();
        }
    }

}
