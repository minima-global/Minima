package org.minima.tests.utils;

import org.minima.utils.digest.KeccakDigest;

import static org.junit.Assert.assertArrayEquals;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.utils.BaseConverter;

/**
 * Keccak Test
 */
public class KeccakTests {

    final static String[] messages = {"",
        "54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67",
        "54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f672e"};

    final static String[] digests288 = { // the default settings
        "6753e3380c09e385d0339eb6b050a68f66cfd60a73476e6fd6adeb72f5edd7c6f04a5d01", // message[0]
        "0bbe6afae0d7e89054085c1cc47b1689772c89a41796891e197d1ca1b76f288154933ded", // message[1]
        "82558a209b960ddeb531e6dcb281885b2400ca160472462486e79f071e88a3330a8a303d", // message[2]
        "94049e1ad7ef5d5b0df2b880489e7ab09ec937c3bfc1b04470e503e1ac7b1133c18f86da", // 64k a-test
        "a9cb5a75b5b81b7528301e72553ed6770214fa963956e790528afe420de33c074e6f4220", // random alphabet test
        "eadaf5ba2ad6a2f6f338fce0e1efdad2a61bb38f6be6068b01093977acf99e97a5d5827c" // extremely long data test
    };

    final static String[] digests224 = {"f71837502ba8e10837bdd8d365adb85591895602fc552b48b7390abd",
        "310aee6b30c47350576ac2873fa89fd190cdc488442f3ef654cf23fe",
        "c59d4eaeac728671c635ff645014e2afa935bebffdb5fbd207ffdeab",
        "f621e11c142fbf35fa8c22841c3a812ba1e0151be4f38d80b9f1ff53",
        "68b5fc8c87193155bba68a2485377e809ee4f81a85ef023b9e64add0",
        "c42e4aee858e1a8ad2976896b9d23dd187f64436ee15969afdbc68c5"};

    final static String[] digests256 = {"c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470",
        "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15",
        "578951e24efd62a3d63a86f7cd19aaa53c898fe287d2552133220370240b572d",
        "0047a916daa1f92130d870b542e22d3108444f5a7e4429f05762fb647e6ed9ed",
        "db368762253ede6d4f1db87e0b799b96e554eae005747a2ea687456ca8bcbd03",
        "5f313c39963dcf792b5470d4ade9f3a356a3e4021748690a958372e2b06f82a4"};

    final static String[] digests384 = {
        "2c23146a63a29acf99e73b88f8c24eaa7dc60aa771780ccc006afbfa8fe2479b2dd2b21362337441ac12b515911957ff",
        "283990fa9d5fb731d786c5bbee94ea4db4910f18c62c03d173fc0a5e494422e8a0b3da7574dae7fa0baf005e504063b3",
        "9ad8e17325408eddb6edee6147f13856ad819bb7532668b605a24a2d958f88bd5c169e56dc4b2f89ffd325f6006d820b",
        "c704cfe7a1a53208ca9526cd24251e0acdc252ecd978eee05acd16425cfb404ea81f5a9e2e5e97784d63ee6a0618a398",
        "d4fe8586fd8f858dd2e4dee0bafc19b4c12b4e2a856054abc4b14927354931675cdcaf942267f204ea706c19f7beefc4",
        "9b7168b4494a80a86408e6b9dc4e5a1837c85dd8ff452ed410f2832959c08c8c0d040a892eb9a755776372d4a8732315"};

    final static String[] digests512 = {
        "0eab42de4c3ceb9235fc91acffe746b29c29a8c366b7c60e4e67c466f36a4304c00fa9caf9d87976ba469bcbe06713b435f091ef2769fb160cdab33d3670680e",
        "d135bb84d0439dbac432247ee573a23ea7d3c9deb2a968eb31d47c4fb45f1ef4422d6c531b5b9bd6f449ebcc449ea94d0a8f05f62130fda612da53c79659f609",
        "ab7192d2b11f51c7dd744e7b3441febf397ca07bf812cceae122ca4ded6387889064f8db9230f173f6d1ab6e24b6e50f065b039f799f5592360a6558eb52d760",
        "34341ead153aa1d1fdcf6cf624c2b4f6894b6fd16dc38bd4ec971ac0385ad54fafcb2e0ed86a1e509456f4246fdcb02c3172824cd649d9ad54c51f7fb49ea67c",
        "dc44d4f4d36b07ab5fc04016cbe53548e5a7778671c58a43cb379fd00c06719b8073141fc22191ffc3db5f8b8983ae8341fa37f18c1c969664393aa5ceade64e",
        "3e122edaf37398231cfaca4c7c216c9d66d5b899ec1d7ac617c40c7261906a45fc01617a021e5da3bd8d4182695b5cb785a28237cbb167590e34718e56d8aab8"};

    @Test
    public void testKeccakDigest() {
        byte byt = 9;
        int bitLength = 160;
        byte[] bytArray = {1, 0, 1, 0};
        KeccakDigest i = new KeccakDigest();
        assertNotNull(i);
        KeccakDigest j = new KeccakDigest(i);
        assertNotNull(j);
        KeccakDigest k = new KeccakDigest(512);
        k.getByteLength();
        KeccakDigest l = new KeccakDigest(k);
        byte[] hash = new byte[k.getDigestSize()];
        System.out.println(
                "should be equal k:" + k.getByteLength() + " " + k.getDigestSize() + " " + k.getAlgorithmName());
        k.update(bytArray, 9, 0);
        k.update(bytArray, 0, 0);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);

        byte[] k64 = new byte[1024 * 64];

        for (int z = 0; z != 100; z++) {
            k64[z] = (byte) 'q';
            k.update(k64, 0, 9);
        }
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.update(byt);
        k.doFinal(hash, 0);

        // k.doFinal(bytArray, bitLength, byt, 2);
        System.out.println(
                "should be equal k:" + k.getByteLength() + " " + k.getDigestSize() + " " + k.getAlgorithmName());

        // assertTrue("should be equal ", k.getDigestSize() == (512));
        // KeccakDigest.init(160)
        i.update(byt);
        assertNotNull(i.getDigestSize());
        KeccakDigest g = new KeccakDigest();

        assertNotNull(k.getDigestSize());
        // i.update(bytArray, 8, 3);
        j.reset();

        i.toString();
        // assertTrue("should be equal to string 0", i.toString().contentEquals("0"));
        // assertFalse("should be equal to constant zero", i.isEqual(MiniNumber.ONE));
        // assertNotNull("should not be null", i);

    }

    public void testDigest(KeccakDigest digest, String[] input) {

        String[] expected = input;
        KeccakDigest i = new KeccakDigest(digest);
        byte[] hash = new byte[i.getDigestSize()];

        for (int j = 0; j != messages.length; j++) {
            if (messages.length != 0) {
                byte[] data = BaseConverter.decode16(messages[j]);

                i.update(data, 0, data.length);
            }
            i.doFinal(hash, 0);

            try {
                assertArrayEquals(" keccak first round should match hash on " + i.getAlgorithmName() + " index " + j,
                        BaseConverter.decode16(expected[j]), hash);
            } catch (ArrayComparisonFailure failure) {
                System.out.println("Test failed: " + failure.getMessage());
                assertFalse("test should not fail:" + failure.getMessage(), true);
            }
        }
        byte[] k64 = new byte[1024 * 64];

        for (int k = 0; k != k64.length; k++) {
            k64[k] = (byte) 'a';
        }

        i.update(k64, 0, k64.length);

        i.doFinal(hash, 0);

        try {
            assertArrayEquals(" keccak 64k a should match hash on " + i.getAlgorithmName() + " 64ka" + i,
                    BaseConverter.decode16(expected[messages.length]), hash);

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        for (int l = 0; l != k64.length; l++) {
            i.update((byte) 'a');
        }

        i.doFinal(hash, 0);

        try {
            assertArrayEquals(" keccak 64k a single should match hash on " + i.getAlgorithmName() + " 64ka" + i,
                    BaseConverter.decode16(expected[messages.length]), hash);

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        for (int m = 0; m != k64.length; m++) {
            k64[m] = (byte) ('a' + (m % 26));
        }

        i.update(k64, 0, k64.length);

        i.doFinal(hash, 0);
        try {
            assertFalse(" keccak 64k alpha should not match hash on ",
                    BaseConverter.decode16(expected[messages.length] + 1).equals(hash));

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        for (int n = 0; n != 64; n++) {
            i.update(k64[n * 1024]);
            i.update(k64, n * 1024 + 1, 1023);
        }

        i.doFinal(hash, 0);
        try {
            assertFalse(" keccak 64k chunked alpha should not match hash on ",
                    BaseConverter.decode16(expected[messages.length] + 1).equals(hash));

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        i.doFinal(hash, 0);

        // byte[] newHash = new byte[64];
        // long start = System.currentTimeMillis();
        // System.out.println("Starting very long");
        // for (int z = 0; z != 16384; z++) {
        // for (int j = 0; j != 1024; j++) {
        // i.update(newHash, 0, newHash.length);
        // }
        // }
        // i.doFinal(newHash, 0);
        // if (!Arrays.areEqual(newHash, hash)) {
        // assertNotNull(i.getAlgorithmName());
        // System.out.println("Keccak mismatch on " + i.getAlgorithmName() + " extreme
        // data test");
        // }
        // System.out.println("Done " + (System.currentTimeMillis() - start));
    }

    @Test

    public void testDigestDoFinal() {
        KeccakDigest kd = new KeccakDigest();

        byte[] hash = new byte[kd.getDigestSize()];
        kd.doFinal(hash, 0);

        for (int i = 0; i <= kd.getDigestSize(); ++i) {
            byte[] cmp = new byte[2 * kd.getDigestSize()];
            System.arraycopy(hash, 0, cmp, i, hash.length);

            byte[] buf = new byte[2 * kd.getDigestSize()];
            kd.doFinal(buf, i);

            try {
                assertArrayEquals(" keccak 64k a single should match hash on " + kd.getAlgorithmName() + " 64ka" + i,
                        cmp, buf);

            } catch (ArrayComparisonFailure failure) {
                System.out.println("Test failed: " + failure.getMessage());
                assertFalse("test should not fail:" + failure.getMessage(), true);
            }
        }
    }

    @Test

    public void testRunKeccakVariants() {
        testDigest(new KeccakDigest(), digests288);
        testDigest(new KeccakDigest(224), digests224);
        testDigest(new KeccakDigest(256), digests256);
        testDigest(new KeccakDigest(384), digests384);
    }

}
