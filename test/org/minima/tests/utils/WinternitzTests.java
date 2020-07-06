package org.minima.tests.utils;

import org.minima.utils.digest.WinternitzOTSVerify;
import org.minima.utils.digest.WinternitzOTSignature;
import org.minima.utils.digest.KeccakDigest;

import static org.junit.Assert.assertArrayEquals;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.utils.BaseConverter;

/**
 * Winternitz Test
 */
public class WinternitzTests {

        // winternitz parameter - minima currently uses 12
        final static int w = 12;
        final static int bitLength = w*3*8;

    final static String[] messages = { "",
            "54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67",
            "54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f672e" };

    final static String[] digests288 = { // the default settings
            "6753e3380c09e385d0339eb6b050a68f66cfd60a73476e6fd6adeb72f5edd7c6f04a5d01", // message[0]
            "0bbe6afae0d7e89054085c1cc47b1689772c89a41796891e197d1ca1b76f288154933ded", // message[1]
            "82558a209b960ddeb531e6dcb281885b2400ca160472462486e79f071e88a3330a8a303d", // message[2]
            "94049e1ad7ef5d5b0df2b880489e7ab09ec937c3bfc1b04470e503e1ac7b1133c18f86da", // 64k a-test
            "a9cb5a75b5b81b7528301e72553ed6770214fa963956e790528afe420de33c074e6f4220", // random alphabet test
            "eadaf5ba2ad6a2f6f338fce0e1efdad2a61bb38f6be6068b01093977acf99e97a5d5827c" // extremely long data test
    };

    @Test
    public void testWinternitzSign() {
                // pseudorandom generator seed
                byte[] pSeed = new byte[w*3];
                for(int i = 0; i < w; i++) {
                        pSeed[i] = 0;
                }

                //Create a WOTS
                WinternitzOTSignature wots = new WinternitzOTSignature(pSeed, new KeccakDigest(bitLength), w);
                wots.getPublicKey();
                
                byte[][] privateKeyOTS = wots.getPrivateKey();
                assertTrue("there should be at least one private key", privateKeyOTS.length > 0);
                int dLength = privateKeyOTS[0].length;
                assertTrue("digest length should be bigger than zero", dLength > 0);
                assertTrue("digest length should match bitLength", dLength == bitLength/8);
                for(int pkey = 0; pkey < privateKeyOTS.length; pkey++) {
                        assertTrue("each private key length should match bytelength", 
                        privateKeyOTS[pkey].length == bitLength / 8);
                        int abskeysum = 0;
                        for(int i = 0; i < privateKeyOTS[pkey].length; i++) {
                                abskeysum = abskeysum + Math.abs(privateKeyOTS[pkey][i]);
                                // keysum = keysum + privateKeyOTS[pkey][i];
                                //System.out.println(" pkey="+pkey+" i="+i + ": " + privateKeyOTS[pkey][i]);
                        }
                        //System.out.println("abskeysum: " + abskeysum);
                        assertTrue("private key should not be equal to zero", abskeysum > 0);
                }
                //Get the Public Key..
                //mPublicKey  = new MiniData(wots.getPublicKey());
                byte[] pubkey = wots.getPublicKey();
                assertTrue("pubkey should not be zero length", pubkey.length > 0);
                int abspubkeysum = 0;
                System.out.println("pubkey length = " + pubkey.length);
                assertTrue("pubkey length = privkey length", pubkey.length == w*3);
                for(int i = 0; i < pubkey.length; i++) {
                        abspubkeysum = abspubkeysum + pubkey[i];
                }
                assertTrue("abspubkeysum is bigger than zero", abspubkeysum > 0);

                // sign the null message

                byte[] sig0 = wots.getSignature(messages[0].getBytes());
                int absSum = 0;
                for(int i = 0; i < sig0.length; i++) {
                        absSum = absSum + Math.abs(sig0[i]);
                }
                assertTrue("signature should not be zero", absSum > 0);

                 // signs message 1
                byte[] sig1 = wots.getSignature(messages[1].getBytes());
                absSum = 0;
                for(int i = 0; i < sig1.length; i++) {
                        absSum = absSum + Math.abs(sig0[i]);
                }
                assertTrue("signature should not be zero", absSum > 0);
                WinternitzOTSVerify verify = new WinternitzOTSVerify(new KeccakDigest(bitLength), w);
                byte[] otsPubKey = verify.Verify(messages[1].getBytes(), sig1);
                // pub key from message should match pub key in signature
                try {
                assertArrayEquals("Pub key derived from signed message should match initial pub key", pubkey, otsPubKey);
                } catch(ArrayComparisonFailure error) {
                        System.out.println("error - derived pub key does not match initial pub key");
                        System.out.println("Error: " + error.getMessage());
                        assertTrue("should never throw", false);
                }
                // signs message 
                byte[] sig2 = wots.getSignature(new String("The Times 03/Jan/2009 Chancellor on brink of second bailout for banks").getBytes());
                absSum = 0;
                for(int i = 0; i < sig2.length; i++) {
                        absSum = absSum + Math.abs(sig0[i]);
                }
                assertTrue("signature should not be zero", absSum > 0);
                otsPubKey = verify.Verify(messages[1].getBytes(), sig1);
                // pub key from message should match pub key in signature
                try {
                assertArrayEquals("Pub key derived from signed message should match initial pub key", pubkey, otsPubKey);
                } catch(ArrayComparisonFailure error) {
                        System.out.println("error - derived pub key does not match initial pub key");
                        System.out.println("Error: " + error.getMessage());
                        assertTrue("should never throw", false);
                }
                
                // signs message lorem ipsum
                String loremString = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
                byte[] sig3 = wots.getSignature(loremString.getBytes());
                absSum = 0;
                for(int i = 0; i < sig3.length; i++) {
                        absSum = absSum + Math.abs(sig0[i]);
                }
                assertTrue("signature should not be zero", absSum > 0);
                otsPubKey = verify.Verify(messages[1].getBytes(), sig1);
                // pub key from message should match pub key in signature
                try {
                assertArrayEquals("Pub key derived from signed message should match initial pub key", pubkey, otsPubKey);
                } catch(ArrayComparisonFailure error) {
                        System.out.println("error - derived pub key does not match initial pub key");
                        System.out.println("Error: " + error.getMessage());
                        assertTrue("should never throw", false);
                }
    }
    
    //@Test
    // public void testKeccakDigest() {
    //     byte byt = 9;
    //     int bitLength = 160;
    //     byte[] bytArray = { 1, 0, 1, 0 };
    //     KeccakDigest i = new KeccakDigest();
    //     assertNotNull(i);
    //     KeccakDigest j = new KeccakDigest(i);
    //     assertNotNull(j);
    //     KeccakDigest k = new KeccakDigest(512);
    //     k.getByteLength();
    //     KeccakDigest l = new KeccakDigest(k);
    //     byte[] hash = new byte[k.getDigestSize()];
    //     System.out.println(
    //             "should be equal k:" + k.getByteLength() + " " + k.getDigestSize() + " " + k.getAlgorithmName());
    //     k.update(bytArray, 9, 0);
    //     k.update(bytArray, 0, 0);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);

    //     byte[] k64 = new byte[1024 * 64];

    //     for (int z = 0; z != 100; z++) {
    //         k64[z] = (byte) 'q';
    //         k.update(k64, 0, 9);
    //     }
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.update(byt);
    //     k.doFinal(hash, 0);

    //     // k.doFinal(bytArray, bitLength, byt, 2);
    //     System.out.println(
    //             "should be equal k:" + k.getByteLength() + " " + k.getDigestSize() + " " + k.getAlgorithmName());

    //     // assertTrue("should be equal ", k.getDigestSize() == (512));

    //     // KeccakDigest.init(160)
    //     i.update(byt);
    //     assertNotNull(i.getDigestSize());
    //     KeccakDigest g = new KeccakDigest();

    //     assertNotNull(k.getDigestSize());
    //     // i.update(bytArray, 8, 3);
    //     j.reset();

    //     i.toString();
    //     // assertTrue("should be equal to string 0", i.toString().contentEquals("0"));
    //     // assertFalse("should be equal to constant zero", i.isEqual(MiniNumber.ONE));
    //     // assertNotNull("should not be null", i);

    // }

    // public void testDigest(KeccakDigest digest, String[] input) {

    //     String[] expected = input;
    //     KeccakDigest i = new KeccakDigest(digest);
    //     byte[] hash = new byte[i.getDigestSize()];

    //     for (int j = 0; j != messages.length; j++) {
    //         if (messages.length != 0) {
    //             byte[] data = BaseConverter.decode16(messages[j]);

    //             i.update(data, 0, data.length);
    //         }
    //         i.doFinal(hash, 0);

    //         try {
    //             assertArrayEquals(" keccak first round should match hash on " + i.getAlgorithmName() + " index " + j,
    //                     BaseConverter.decode16(expected[j]), hash);
    //         } catch (ArrayComparisonFailure failure) {
    //             System.out.println("Test failed: " + failure.getMessage());
    //             assertFalse("test should not fail:" + failure.getMessage(), true);
    //         }
    //     }
    //     byte[] k64 = new byte[1024 * 64];

    //     for (int k = 0; k != k64.length; k++) {
    //         k64[k] = (byte) 'a';
    //     }

    //     i.update(k64, 0, k64.length);

    //     i.doFinal(hash, 0);

    //     try {
    //         assertArrayEquals(" keccak 64k a should match hash on " + i.getAlgorithmName() + " 64ka" + i,
    //                 BaseConverter.decode16(expected[messages.length]), hash);

    //     } catch (ArrayComparisonFailure failure) {
    //         System.out.println("Test failed: " + failure.getMessage());
    //         assertFalse("test should not fail:" + failure.getMessage(), true);
    //     }

    //     for (int l = 0; l != k64.length; l++) {
    //         i.update((byte) 'a');
    //     }

    //     i.doFinal(hash, 0);

    //     try {
    //         assertArrayEquals(" keccak 64k a single should match hash on " + i.getAlgorithmName() + " 64ka" + i,
    //                 BaseConverter.decode16(expected[messages.length]), hash);

    //     } catch (ArrayComparisonFailure failure) {
    //         System.out.println("Test failed: " + failure.getMessage());
    //         assertFalse("test should not fail:" + failure.getMessage(), true);
    //     }

    //     for (int m = 0; m != k64.length; m++) {
    //         k64[m] = (byte) ('a' + (m % 26));
    //     }

    //     i.update(k64, 0, k64.length);

    //     i.doFinal(hash, 0);
    //     try {
    //         assertFalse(" keccak 64k alpha should not match hash on ",
    //                 BaseConverter.decode16(expected[messages.length] + 1).equals(hash));

    //     } catch (ArrayComparisonFailure failure) {
    //         System.out.println("Test failed: " + failure.getMessage());
    //         assertFalse("test should not fail:" + failure.getMessage(), true);
    //     }

    //     for (int n = 0; n != 64; n++) {
    //         i.update(k64[n * 1024]);
    //         i.update(k64, n * 1024 + 1, 1023);
    //     }

    //     i.doFinal(hash, 0);
    //     try {
    //         assertFalse(" keccak 64k chunked alpha should not match hash on ",
    //                 BaseConverter.decode16(expected[messages.length] + 1).equals(hash));

    //     } catch (ArrayComparisonFailure failure) {
    //         System.out.println("Test failed: " + failure.getMessage());
    //         assertFalse("test should not fail:" + failure.getMessage(), true);
    //     }

    //     i.doFinal(hash, 0);

    //     // byte[] newHash = new byte[64];

    //     // long start = System.currentTimeMillis();
    //     // System.out.println("Starting very long");
    //     // for (int z = 0; z != 16384; z++) {
    //     // for (int j = 0; j != 1024; j++) {
    //     // i.update(newHash, 0, newHash.length);
    //     // }
    //     // }

    //     // i.doFinal(newHash, 0);

    //     // if (!Arrays.areEqual(newHash, hash)) {
    //     // assertNotNull(i.getAlgorithmName());
    //     // System.out.println("Keccak mismatch on " + i.getAlgorithmName() + " extreme
    //     // data test");
    //     // }
    //     // System.out.println("Done " + (System.currentTimeMillis() - start));
    // }

    // @Test

    // public void testDigestDoFinal() {
    //     KeccakDigest kd = new KeccakDigest();

    //     byte[] hash = new byte[kd.getDigestSize()];
    //     kd.doFinal(hash, 0);

    //     for (int i = 0; i <= kd.getDigestSize(); ++i) {
    //         byte[] cmp = new byte[2 * kd.getDigestSize()];
    //         System.arraycopy(hash, 0, cmp, i, hash.length);

    //         byte[] buf = new byte[2 * kd.getDigestSize()];
    //         kd.doFinal(buf, i);

    //         try {
    //             assertArrayEquals(" keccak 64k a single should match hash on " + kd.getAlgorithmName() + " 64ka" + i,
    //                     cmp, buf);

    //         } catch (ArrayComparisonFailure failure) {
    //             System.out.println("Test failed: " + failure.getMessage());
    //             assertFalse("test should not fail:" + failure.getMessage(), true);
    //         }
    //     }
    // }

    // @Test

    // public void testRunKeccakVariants() {
    //     testDigest(new KeccakDigest(), digests288);
    //     testDigest(new KeccakDigest(224), digests224);
    //     testDigest(new KeccakDigest(256), digests256);
    //     testDigest(new KeccakDigest(384), digests384);
    // }

}
