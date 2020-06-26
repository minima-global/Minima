package org.minima.tests.utils;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.utils.BaseConverter;
import java.util.Arrays;

// import org.bouncycastle.util.Arrays;
import org.minima.utils.digest.Digest;
import org.minima.utils.digest.Memoable;
import org.minima.utils.digest.EncodableDigest;
import org.minima.utils.digest.SHA256Digest;

import junit.framework.ComparisonFailure;

public abstract class DigestTest {
    

    private Digest digest;
    private String[] input;
    private String[] results;

    DigestTest(
        Digest digest,
        String[] input,
        String[] results)
    {
        this.digest = digest;
        this.input = input;
        this.results = results;
    }
    
    public String getName()
    {
        return digest.getAlgorithmName();
    }
    
    public void performTest()
    {
        byte[] resBuf = new byte[digest.getDigestSize()];
    
        for (int i = 0; i < input.length - 1; i++)
        {
            byte[] m = toByteArray(input[i]);

            vectorTest(digest, i, resBuf, m, BaseConverter.decode16(results[i]));
        }

        offsetTest(digest, 0, toByteArray(input[0]), BaseConverter.decode16(results[0]));

        byte[] lastV = toByteArray(input[input.length - 1]);
        byte[] lastDigest = BaseConverter.decode16(results[input.length - 1]);
        
        vectorTest(digest, input.length - 1, resBuf, lastV, BaseConverter.decode16(results[input.length - 1]));

        testClone(resBuf, lastV, lastDigest);
        testMemo(resBuf, lastV, lastDigest);
        if (digest instanceof EncodableDigest)
        {
            testEncodedState(resBuf, lastV, lastDigest);
        }
    }

    private void testEncodedState(byte[] resBuf, byte[] input, byte[] expected)
    {
        // test state encoding;
        digest.update(input, 0, input.length / 2);

        // copy the Digest
        Digest copy1 = cloneDigest(((EncodableDigest)digest).getEncodedState());
        Digest copy2 = cloneDigest(((EncodableDigest)copy1).getEncodedState());

        digest.update(input, input.length / 2, input.length - input.length / 2);

        digest.doFinal(resBuf, 0);

        try {
            assertArrayEquals(" vector test should match hash on " + expected + " 64ka" + resBuf,
            expected, resBuf);

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        copy1.update(input, input.length / 2, input.length - input.length / 2);
        copy1.doFinal(resBuf, 0);

        try {
            assertArrayEquals(" state copy1 vector test should match on " + expected + " resBuf:" + resBuf,
            expected, resBuf);

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
       

        copy2.update(input, input.length / 2, input.length - input.length / 2);
        copy2.doFinal(resBuf, 0);
        try {
            assertArrayEquals(" state copy2 vector test should match on " + expected + " resBuf:" + resBuf,
            expected, resBuf);

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        
    }

    private void testMemo(byte[] resBuf, byte[] input, byte[] expected)
    {
        Memoable m = (Memoable)digest;

        digest.update(input, 0, input.length/2);

        // copy the Digest
        Memoable copy1 = m.copy();
        Memoable copy2 = copy1.copy();

        digest.update(input, input.length/2, input.length - input.length/2);
        digest.doFinal(resBuf, 0);
        try {
            assertTrue(" memo vector test should match on " + expected + " resBuf:" + resBuf,
            results[results.length - 1].equals(new String(BaseConverter.encode16(resBuf))));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        m.reset(copy1);

        digest.update(input, input.length/2, input.length - input.length/2);
        digest.doFinal(resBuf, 0);
        try {

            assertTrue(" should equal " + expected + " resBuf:" + resBuf, !expected.equals(resBuf));
            assertTrue(" memo reset vector test should match on " + expected + " resBuf:" + resBuf,
            results[results.length - 1].equals(new String(BaseConverter.encode16(resBuf))));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
       

        Digest md = (Digest)copy2;

        md.update(input, input.length/2, input.length - input.length/2);
        md.doFinal(resBuf, 0);
        try {

            assertTrue(" should equal " + expected + " resBuf:" + resBuf, !expected.equals(resBuf));
            assertTrue(" memo copy vector test should match on " + expected + " resBuf:" + resBuf,
            results[results.length - 1].equals(new String(BaseConverter.encode16(resBuf))));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
        
    }

    private void testClone(byte[] resBuf, byte[] input, byte[] expected)
    {
        digest.update(input, 0, input.length / 2);

        // clone the Digest
        Digest d = cloneDigest(digest);

        digest.update(input, input.length/2, input.length - input.length/2);
        digest.doFinal(resBuf, 0);
        try {

            assertTrue(" should equal " + expected + " resBuf:" + resBuf, !expected.equals(resBuf));
            assertTrue(" clone vector test should match on " + expected + " resBuf:" + resBuf,
            results[results.length - 1].equals(new String(BaseConverter.encode16(resBuf))));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        d.update(input, input.length/2, input.length - input.length/2);
        d.doFinal(resBuf, 0);
        try {

            assertTrue(" should equal " + expected + " resBuf:" + resBuf, !expected.equals(resBuf));
            assertTrue(" second clone vector test should match on " + expected + " resBuf:" + resBuf,
            results[results.length - 1].equals(new String(BaseConverter.encode16(resBuf))));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
    }

    protected byte[] toByteArray(String input)
    {
        byte[] bytes = new byte[input.length()];
        
        for (int i = 0; i != bytes.length; i++)
        {
            bytes[i] = (byte)input.charAt(i);
        }
        
        return bytes;
    }
    
    private void vectorTest(
        Digest digest,
        int count,
        byte[] resBuf,
        byte[] input,
        byte[] expected)
    {
        digest.update(input, 0, input.length);
        digest.doFinal(resBuf, 0);
        try {

            assertTrue(" Vector " + count + " should equal " + expected + " resBuf:" + resBuf, !expected.equals(resBuf));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
        // if (!expected.equals(resBuf))
        // {
        //     fail("Vector " + count + " failed got " + new String(BaseConverter.encode16(resBuf)));
        // }
    }

    private void offsetTest(
        Digest digest,
        int count,
        byte[] input,
        byte[] expected)
    {
        byte[] resBuf = new byte[expected.length + 11];

        digest.update(input, 0, input.length);
        digest.doFinal(resBuf, 11);
        try {
            assertArrayEquals(" Offset " + count + " test should match hash on " + expected + " 64ka" + resBuf,
            Arrays.copyOfRange(resBuf, 11, resBuf.length), expected);

        } catch (ArrayComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
        // if (!areEqual(Arrays.copyOfRange(resBuf, 11, resBuf.length), expected))
        // {
        //     fail("Offset " + count + " failed got " + new String(BaseConverter.encode16(resBuf)));
        // }
    }

    protected abstract Digest cloneDigest(Digest digest);

    protected Digest cloneDigest(byte[] encodedState)
    {
        throw new IllegalStateException("Unsupported");
    }

    //
    // optional tests
    //
    protected void millionATest(
        String expected)
    {
        byte[] resBuf = new byte[digest.getDigestSize()];
        
        for (int i = 0; i < 1000000; i++)
        {
            digest.update((byte)'a');
        }
        
        digest.doFinal(resBuf, 0);

        try {
            assertArrayEquals(" SHA millionATest",
                    BaseConverter.decode16(expected), resBuf);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
    }
    
    protected void sixtyFourKTest(
        String expected)
    {
        byte[] resBuf = new byte[digest.getDigestSize()];
        
        for (int i = 0; i < 65536; i++)
        {
            digest.update((byte)(i & 0xff));
        }
        
        digest.doFinal(resBuf, 0);
        try {

            assertTrue(" 64k test " + expected + " resBuf:" + resBuf, !BaseConverter.decode16(expected).equals(resBuf));

        } catch (ComparisonFailure failure) {
            System.out.println("Expected: " + new String(BaseConverter.encode16(resBuf)));
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
    }

}