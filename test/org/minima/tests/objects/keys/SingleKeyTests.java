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
import org.minima.objects.keys.SingleKey;
import org.minima.utils.json.JSONObject;

public class SingleKeyTests {

    @Test
    public void testConstructors() {
        {
            SingleKey sk = new SingleKey();
            assertThrows(NullPointerException.class, () -> { // uninitialized members
                MiniNumber.ONE.isEqual(sk.getLevel());
            });
        }

        {
            MiniData RndMD = MiniData.getRandomData(32);
            SingleKey sk = new SingleKey(RndMD);

            assertTrue(MiniNumber.ONE.isEqual(sk.getLevel()));
            assertTrue(MiniNumber.ONE.isEqual(sk.getMaxUses()));
            assertTrue(MiniNumber.ONE.isEqual(sk.getTotalAllowedUses()));
            assertTrue(MiniNumber.ZERO.isEqual(sk.getUses()));
            assertTrue(sk.getBitLength() == 256);
        }
    }

    @Test
    public void testSignAndVerification() {
        MiniData RndMD = MiniData.getRandomData(32);
        SingleKey sk = new SingleKey(RndMD);

        {
            MiniData DataToSign = MiniData.getRandomData(256);
            MiniData Signature = sk.sign(DataToSign);
            assertTrue(sk.verify(DataToSign, Signature));
        }
        {
            // Multiple signing allowed, regardless of max use
            MiniData DataToSign = MiniData.getRandomData(256);
            MiniData Signature = sk.sign(DataToSign);
            assertTrue(sk.verify(DataToSign, Signature));
        }
    }

}
