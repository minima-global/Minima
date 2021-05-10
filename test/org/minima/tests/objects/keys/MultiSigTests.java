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

import org.minima.database.mmr.MMRSet;
import org.minima.objects.Magic;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.MultiKey;
import org.minima.objects.keys.MultiSig;
import org.minima.objects.keys.SingleKey;
import org.minima.objects.proofs.Proof;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class MultiSigTests {

    @Test
    public void testConstructors() {
        {
            MiniData zPrivateSeed = MiniData.getRandomData(32);
            MiniNumber zKeysPerLevel = MultiKey.DEFAULT_KEYS_PER_LEVEL;
            MiniNumber zLevel = MultiKey.DEFAULT_LEVELS;
            MiniNumber zBitLength = new MiniNumber(zPrivateSeed.getLength() * 8);
            SingleKey[] zSingleKeys = new SingleKey[zKeysPerLevel.getAsInt()];
            MMRSet zMMRset = new MMRSet(null, zBitLength.getAsInt());

            for (int i = 0; i < zKeysPerLevel.getAsInt(); i++) {
                MiniData numberdata = new MiniData(BaseConverter.numberToHex(i));
                MiniData newdata = numberdata.concat(zPrivateSeed);
                byte[] hashdata = Crypto.getInstance().hashData(newdata.getData(), zBitLength.getAsInt());
                MiniData spriv = new MiniData(hashdata);
                zSingleKeys[i] = new SingleKey(spriv);
                zMMRset.addLeafNode(zSingleKeys[i].getPublicKey());
            }

            zMMRset.finalizeSet();

            MiniData zDataToSign = MiniData.getRandomData(256);

            MiniData zCurrentPublicKey = zSingleKeys[0].getPublicKey();
            MiniData zCurrentSignature = zSingleKeys[0].sign(zDataToSign);
            MiniData zCurrentProof = zMMRset.getProof(new MiniNumber(0)).getChainSHAProof();

            MultiSig zSig = new MultiSig(zCurrentPublicKey, zCurrentProof, zCurrentSignature);

            Proof chainproof = new Proof();
            chainproof.setData(zCurrentPublicKey);
            chainproof.setProof(zCurrentProof);
            MiniData zRootKey = chainproof.getFinalHash();

            assertTrue(zCurrentPublicKey.isEqual(zSig.getPublicKey()));
            assertTrue(zCurrentSignature.isEqual(zSig.getSignature()));
            assertTrue(zCurrentProof.isEqual(zSig.getProofChain()));
            assertFalse(zSig.hasChildSignature());
            assertNull(zSig.getChildSignature());
            assertTrue(zRootKey.isEqual(zSig.getRootKey()));

        }
    }

    @Test
    public void test() {
    }

    @Test
    public void testJSONConversion() {
        MiniData zPrivateSeed = MiniData.getRandomData(32);
        MiniNumber zKeysPerLevel = MultiKey.DEFAULT_KEYS_PER_LEVEL;
        MiniNumber zLevel = MultiKey.DEFAULT_LEVELS;
        MiniNumber zBitLength = new MiniNumber(zPrivateSeed.getLength() * 8);
        SingleKey[] zSingleKeys = new SingleKey[zKeysPerLevel.getAsInt()];
        MMRSet zMMRset = new MMRSet(null, zBitLength.getAsInt());

        for (int i = 0; i < zKeysPerLevel.getAsInt(); i++) {
            MiniData numberdata = new MiniData(BaseConverter.numberToHex(i));
            MiniData newdata = numberdata.concat(zPrivateSeed);
            byte[] hashdata = Crypto.getInstance().hashData(newdata.getData(), zBitLength.getAsInt());
            MiniData spriv = new MiniData(hashdata);
            zSingleKeys[i] = new SingleKey(spriv);
            zMMRset.addLeafNode(zSingleKeys[i].getPublicKey());
        }

        zMMRset.finalizeSet();

        MiniData zDataToSign = MiniData.getRandomData(256);

        MiniData zCurrentPublicKey = zSingleKeys[0].getPublicKey();
        MiniData zCurrentSignature = zSingleKeys[0].sign(zDataToSign);
        MiniData zCurrentProof = zMMRset.getProof(new MiniNumber(0)).getChainSHAProof();

        MultiSig zSig = new MultiSig(zCurrentPublicKey, zCurrentProof, zCurrentSignature);

        JSONObject json = zSig.toJSON();

        assertTrue("JSON object should contain publickey key", json.containsKey("publickey"));
        assertTrue("JSON object should contain proof key", json.containsKey("proof"));
        assertTrue("JSON object should contain signature key", json.containsKey("signature"));
        assertTrue("JSON object should contain childsig key", json.containsKey("childsig"));
    }

    @Test
    public void testToString() {
        MiniData zPrivateSeed = MiniData.getRandomData(32);
        MiniNumber zKeysPerLevel = MultiKey.DEFAULT_KEYS_PER_LEVEL;
        MiniNumber zLevel = MultiKey.DEFAULT_LEVELS;
        MiniNumber zBitLength = new MiniNumber(zPrivateSeed.getLength() * 8);
        SingleKey[] zSingleKeys = new SingleKey[zKeysPerLevel.getAsInt()];
        MMRSet zMMRset = new MMRSet(null, zBitLength.getAsInt());

        for (int i = 0; i < zKeysPerLevel.getAsInt(); i++) {
            MiniData numberdata = new MiniData(BaseConverter.numberToHex(i));
            MiniData newdata = numberdata.concat(zPrivateSeed);
            byte[] hashdata = Crypto.getInstance().hashData(newdata.getData(), zBitLength.getAsInt());
            MiniData spriv = new MiniData(hashdata);
            zSingleKeys[i] = new SingleKey(spriv);
            zMMRset.addLeafNode(zSingleKeys[i].getPublicKey());
        }

        zMMRset.finalizeSet();

        MiniData zDataToSign = MiniData.getRandomData(256);

        MiniData zCurrentPublicKey = zSingleKeys[0].getPublicKey();
        MiniData zCurrentSignature = zSingleKeys[0].sign(zDataToSign);
        MiniData zCurrentProof = zMMRset.getProof(new MiniNumber(0)).getChainSHAProof();

        MultiSig zSig = new MultiSig(zCurrentPublicKey, zCurrentProof, zCurrentSignature);

        String exp_s = zSig.toJSON().toString();
        String obj_s = zSig.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
