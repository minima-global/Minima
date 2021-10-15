package keys;

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
import org.minima.objects.keys.MultiKey;
import org.minima.utils.json.JSONObject;

public class MultiKeyTests {

    @Test
    public void testConstructors() {
        {
            MultiKey mk = new MultiKey();
            assertThrows(NullPointerException.class, () -> { // uninitialized members
                MiniNumber.ONE.isEqual(mk.getLevel());
            });
        }

        {
            MiniData RndMD = MiniData.getRandomData(32);
            MultiKey mk = new MultiKey(RndMD);

            assertTrue(RndMD.isEqual(mk.getPublicKey()));
        }

        {
            MultiKey mk = new MultiKey(256);

            assertTrue(MultiKey.DEFAULT_LEVELS.isEqual(mk.getLevel()));
            assertTrue(MultiKey.DEFAULT_KEYS_PER_LEVEL.isEqual(mk.getMaxUses()));
            assertTrue(MultiKey.DEFAULT_KEYS_PER_LEVEL.pow(MultiKey.DEFAULT_LEVELS.getAsInt()).isEqual(mk.getTotalAllowedUses()));
            assertTrue(MiniNumber.ZERO.isEqual(mk.getUses()));
            assertTrue(mk.getBitLength() == 256);
        }

        {
            int BitLength = 512;
            MiniNumber KeysPerLevel = new MiniNumber(4);
            MiniNumber Level = new MiniNumber(4);

            MultiKey mk = new MultiKey(BitLength, KeysPerLevel, Level);

            assertTrue(Level.isEqual(mk.getLevel()));
            assertTrue(KeysPerLevel.isEqual(mk.getMaxUses()));
            assertTrue(KeysPerLevel.pow(Level.getAsInt()).isEqual(mk.getTotalAllowedUses()));
            assertTrue(MiniNumber.ZERO.isEqual(mk.getUses()));
            assertTrue(mk.getBitLength() == 512);
        }
    }

    @Test
    public void testSignAndVerification() {
        int BitLength = 160;
        MiniNumber KeysPerLevel = new MiniNumber(4);
        MiniNumber Level = new MiniNumber(4);

        MultiKey mk = new MultiKey(BitLength, KeysPerLevel, Level);

        {
            MiniData DataToSign = MiniData.getRandomData(256);
            MiniData Signature = mk.sign(DataToSign);
            assertTrue(mk.verify(DataToSign, Signature));
        }
        {
            for (int i = 0; i < 1000; i++) {
                // Multiple signing allowed, regardless of max use
                MiniData DataToSign = MiniData.getRandomData(256);
                MiniData Signature = mk.sign(DataToSign);
                assertTrue(mk.verify(DataToSign, Signature));
            }
        }
    }

}
