package org.minima.tests.utils;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.utils.BaseConverter;
import org.minima.utils.digest.Digest;
import org.minima.utils.digest.SHA256Digest;

/**
 * standard vector test for SHA-256 from FIPS Draft 180-2.
 *
 * Note, the first two vectors are _not_ from the draft, the last three are.
 */
public class SHA256Tests extends DigestTest
{
    
        // SHA256Tests(Digest digest, String[] input, String[] results) {
        //     super(digest, input, results);
        //     // TODO Auto-generated constructor stub
        // }

        SHA256Tests(Digest digest, String[] input, String[] results) {
            
            super(digest, input, results);
            // TODO Auto-generated constructor stub
        }

        private static String[] messages =
        {
            "",
            "a",
            "abc",
            "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
        };
        
        private static String[] digests =
        {
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb",
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        };
        
        // 1 million 'a'
        static private String  million_a_digest = "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0";
    
  public void SHA256Test(){

            SHA256Digest sha = new SHA256Digest();
            // super(sha, messages, digests);
            new SHA256Tests(sha, messages, digests);

        }
    
        public void performTest()
        {
            super.performTest();
    
            millionATest(million_a_digest);
        }
    
        protected Digest cloneDigest(Digest digest)
        {
            return new SHA256Digest((SHA256Digest)digest);
        }
    
        protected Digest cloneDigest(byte[] encodedState)
        {
            return new SHA256Digest(encodedState);
        }
    public void testDigest(Digest digest, String[] input) {

        String[] expected = input;
        // SHA256Digest i = new SHA256Digest();
        // SHA256Digest k = new SHA256Digest();

        byte[] hash = new byte[digest.getDigestSize()];

        for (int j = 0; j != messages.length; j++) {
            if (messages.length != 0) {
                byte[] data = BaseConverter.decode16(messages[j]);

                digest.update(data, 0, data.length);
            }
            digest.doFinal(hash, 0);

            try {
                assertArrayEquals(" SHA256 first round should match hash on " + digest.getAlgorithmName() + " index " + j,
                        BaseConverter.decode16(expected[j]), hash);
            } catch (ArrayComparisonFailure failure) {
                System.out.println("Test failed: " + BaseConverter.decode16(digests[j]) + " " + hash);
                System.out.println("Test failed: " + failure.getMessage());

                assertFalse("test should not fail:" + failure.getMessage(), true);
            }
        }
    }

    @Test

    public void testRunSHA256Variants() {
        String[] expected = messages;
       
        // SHA256Digest sha1 = SHA256Digest(digests);
        // SHA256Digest i = new SHA256Digest(digests288);
        Digest keccak1 = new SHA256Digest();
        keccak1.getDigestSize();
        testDigest(keccak1, expected);
        // testDigest(new SHA256Digest(224), digests224);
        // testDigest(new SHA256Digest(256), digests256);
        // testDigest(new SHA256Digest(), digests384);

        // testMac(new SHA256Digest(224), macKeys, macData, mac224, trunc224);
        // testMac(new SHA256Digest(256), macKeys, macData, mac256, trunc256);
        // testMac(new SHA256Digest(384), macKeys, macData, mac384, trunc384);
        // testMac(new SHA256Digest(512), macKeys, macData, mac512, trunc512);
    }
}

