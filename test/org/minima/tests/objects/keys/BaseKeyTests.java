package org.minima.tests.objects.keys;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.minima.objects.Magic;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

import org.minima.tests.objects.keys.BaseKeyDummy;
import org.minima.utils.json.JSONObject;

public class BaseKeyTests {

    @Test
    public void testConstructors() {
        BaseKeyDummy bk = new BaseKeyDummy();
        MiniData RndMD = MiniData.getRandomData(32);
        bk.initKeys(RndMD);

        assertTrue(MiniNumber.ONE.isEqual(bk.getLevel()));
        assertTrue(MiniNumber.ONE.isEqual(bk.getMaxUses()));
        assertTrue(MiniNumber.ZERO.isEqual(bk.getUses()));
    }

    @Test
    public void testGettersAndSetters() {
        BaseKeyDummy bk = new BaseKeyDummy();
        MiniData RndMD = MiniData.getRandomData(32);
        bk.initKeys(RndMD);

        MiniData PubKey = MiniData.getRandomData(32);
        bk.setPublicKey(PubKey);

        assertEquals(PubKey.getLength() * 8, bk.getBitLength());
        assertTrue(PubKey.isEqual(bk.getPublicKey()));

        assertTrue(MiniNumber.ONE.isEqual(bk.getLevel()));
        assertTrue(MiniNumber.ONE.isEqual(bk.getMaxUses()));
        assertTrue(MiniNumber.ZERO.isEqual(bk.getUses()));

        bk.incrementUses();
        assertTrue(MiniNumber.ONE.isEqual(bk.getUses()));
        bk.incrementUses(); // uses increases beyond MaxUses
        assertTrue(MiniNumber.TWO.isEqual(bk.getUses()));
        bk.incrementUses();
        assertTrue((new MiniNumber(3)).isEqual(bk.getUses()));

        bk.setUses(MiniNumber.SIXTYFOUR); // uses set beyond MaxUses
        assertTrue(MiniNumber.SIXTYFOUR.isEqual(bk.getUses()));
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            BaseKeyDummy bk = new BaseKeyDummy();
            bk.initKeys(MiniData.getRandomData(32));

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            bk.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            BaseKeyDummy bk2 = new BaseKeyDummy();
            bk2.readDataStream(dis);

            assertEquals(bk.getBitLength(), bk2.getBitLength());
            assertEquals(bk.getLevel().getAsLong(), bk2.getLevel().getAsLong());
            assertEquals(bk.getMaxUses().getAsLong(), bk2.getMaxUses().getAsLong());
            assertTrue(bk.getPrivateSeed().isEqual(bk2.getPrivateSeed()));
            assertTrue(bk.getPublicKey().isEqual(bk2.getPublicKey()));
            assertEquals(bk.getTotalAllowedUses().getAsLong(), bk2.getTotalAllowedUses().getAsLong());
            assertEquals(bk.getUses().getAsLong(), bk2.getUses().getAsLong());
            assertEquals(bk.getBitLength(), bk2.getBitLength());

        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testJSONConversion() {
        BaseKeyDummy bk = new BaseKeyDummy();
        bk.initKeys(MiniData.getRandomData(32));

        JSONObject json = bk.toJSON();

        assertTrue("JSON object should contain bits key", json.containsKey("bits"));
        assertTrue("JSON object should contain uses key", json.containsKey("uses"));
        assertTrue("JSON object should contain allowed key", json.containsKey("allowed"));
        assertTrue("JSON object should contain publickey key", json.containsKey("publickey"));
    }

    @Test
    public void testToString() {
        BaseKeyDummy bk = new BaseKeyDummy();
        bk.initKeys(MiniData.getRandomData(32));

        String exp_s = bk.getPublicKey().toString();
        String obj_s = bk.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
