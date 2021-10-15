package org.minima.tests.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.minima.objects.Magic;
import org.minima.utils.json.JSONObject;

public class MagicTests {

    @Test
    public void testConstructors() {
        Magic m = new Magic();
    }

    @Test
    public void testReadAndWriteDataStream() {
        try {
            Magic m = new Magic();

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            m.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            Magic m2 = new Magic();
            m2.readDataStream(dis);

            assertTrue(m.mDesiredMaxKISSVMInstructions.isEqual(m2.mDesiredMaxKISSVMInstructions));
            assertTrue(m.mDesiredMaxTxPoWSize.isEqual(m2.mDesiredMaxTxPoWSize));
            assertTrue(m.mDesiredMaxTxnPerBlock.isEqual(m2.mDesiredMaxTxnPerBlock));
            assertEquals(m.mPRNG, m2.mPRNG);
        } catch (final IOException e) {
            fail();
        }
    }

    @Test
    public void testJSONConversion() {
        Magic m = new Magic();

        JSONObject json = m.toJSON();

        assertTrue("JSON object should contain prng key", json.containsKey("prng"));
        assertTrue("JSON object should contain maxtxpow key", json.containsKey("maxtxpow"));
        assertTrue("JSON object should contain maxtxn key", json.containsKey("maxtxn"));
        assertTrue("JSON object should contain maxkissvm key", json.containsKey("maxkissvm"));
    }

    @Test
    public void testToString() {
        Magic m = new Magic();

        String exp_s = m.toJSON().toString();
        String obj_s = m.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
