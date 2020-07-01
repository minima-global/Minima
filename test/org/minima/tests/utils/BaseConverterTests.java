package org.minima.tests.utils;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.utils.BaseConverter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

public class BaseConverterTests {

    final static String[] inputs = { "", "f", "fo", "foo", "foob", "fooba", "foobar", "too", "0x446a1837e14bfec34a9q0141a55ec020f73e15" };

    final static String[] ouputs = { "0x", "0x66", "0x666F", "0x666F6F", "0x666F6F62", "0x666F6F6261",
            "0x666F6F626172" };

    final static String[] ouputs32Hex = { "", "CO======", "CPNG====", "CPNMU===", "CPNMUOG=", "CPNMUOJ1",
            "CPNMUOJ1E8======" };
    final static String[] ouputs32 = { "", "MZXW6===", "MZXW6YQ=", "MZXW6YTB", "MZXW6YTBOI======", "GB4DERRTGJCUGNRRGVBEMQSDGAZTGMSDGFCDIOJUHBBTQQSBGJBUMQRZGU2TGRCBGJDEKNJWGI4TQNJVGAZTAMSDGZCEMOKFHAYUIMZRIU======" };

    @Test
    public void testGeneralBaseCovertor() {
        BaseConverter bd = new BaseConverter();
        assertNotNull(bd);
        System.out.println("should be create instance of BaseConvertor ");
        String hexNum = BaseConverter.numberToHex(8);
        assertNotNull(hexNum);
        System.out.println("convert number into hex");
        String hexNumTwo = BaseConverter.numberToHex(9928);
        assertNotNull(hexNumTwo);
        System.out.println("convert odd number into hex");
        int backToNum = BaseConverter.hexToNumber(hexNumTwo);    
        assertNotNull(backToNum);
        System.out.println("convert hex to int and assign");

    }

    @Test
    public void testBaseEncoder16() {

        byte[] bytTestOne = inputs[0].getBytes();
        String data = BaseConverter.encode16(bytTestOne);

        try {
            assertEquals("should be equal to 0x", data, ouputs[0]);
            System.out.println("should be equal to 0x - " + data);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestTwo = inputs[1].getBytes();
        String dataTwo = BaseConverter.encode16(bytTestTwo);

        try {
            assertEquals("should be equal to 0x66", dataTwo, ouputs[1]);
            System.out.println("should be equal to 0x66 - " + dataTwo);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestThree = inputs[2].getBytes();
        String dataThree = BaseConverter.encode16(bytTestThree);

        try {
            assertEquals("should be equal to 0x666F", dataThree, ouputs[2]);
            System.out.println("should be equal to 0x666F - " + dataThree);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestFour = inputs[3].getBytes();
        String dataFour = BaseConverter.encode16(bytTestFour);

        try {
            assertEquals("should be equal to 0x666F6F", dataFour, ouputs[3]);
            System.out.println("should be equal to 0x666F6F - " + dataFour);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestFive = inputs[4].getBytes();
        String dataFive = BaseConverter.encode16(bytTestFive);

        try {
            assertEquals("should be equal to 0x666F6F62", dataFive, ouputs[4]);
            System.out.println("should be equal to 0x666F6F62 - " + dataFive);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestSix = inputs[5].getBytes();
        String dataSix = BaseConverter.encode16(bytTestSix);

        try {
            assertEquals("should be equal to 0x666F6F6261", dataSix, ouputs[5]);
            System.out.println("should be equal to 0x666F6F6261 - " + dataSix);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestSeven = inputs[6].getBytes();
        String dataSeven = BaseConverter.encode16(bytTestSeven);

        try {
            assertEquals("should be equal to 0x666F6F626172", dataSeven, ouputs[6]);
            System.out.println("should be equal to 0x666F6F626172 - " + dataSeven);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
    }

    @Test
    public void testBaseDecoder16() {

        byte[] data = BaseConverter.decode16(ouputs[0]);
        String decoded = new String(data);

        try {
            assertEquals("should be equal to empty string", decoded, inputs[0]);
            System.out.println("should be equal to empty string - " + decoded);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataTwo = BaseConverter.decode16(ouputs[1]);
        String decodedTwo = new String(dataTwo);

        try {
            assertEquals("should be equal to f", decodedTwo, inputs[1]);
            System.out.println("should be equal to f - " + decodedTwo);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataThree = BaseConverter.decode16(ouputs[2]);
        String decodedThree = new String(dataThree);

        try {
            assertEquals("should be equal to fo", decodedThree, inputs[2]);
            System.out.println("should be equal to fo - " + decodedThree);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataFour = BaseConverter.decode16(ouputs[3]);
        String decodedFour = new String(dataFour);

        try {
            assertEquals("should be equal to foo", decodedFour, inputs[3]);
            System.out.println("should be equal to foo - " + decodedFour);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataFive = BaseConverter.decode16(ouputs[4]);
        String decodedFive = new String(dataFive);

        try {
            assertEquals("should be equal to foob", decodedFive, inputs[4]);
            System.out.println("should be equal to foob - " + decodedFive);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataSix = BaseConverter.decode16(ouputs[5]);
        String decodedSix = new String(dataSix);

        try {
            assertEquals("should be equal to fooba", decodedSix, inputs[5]);
            System.out.println("should be equal to fooba - " + decodedSix);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataSeven = BaseConverter.decode16(ouputs[6]);
        String decodedSeven = new String(dataSeven);

        try {
            assertEquals("should be equal to fooba", decodedSeven, inputs[6]);
            System.out.println("should be equal to fooba - " + decodedSeven);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

    }

    @Test
    public void testBaseEncoder32() {

        byte[] bytTestOne = inputs[0].getBytes();
        String data = BaseConverter.encode32(bytTestOne);

        try {
            assertEquals("should be equal to 0x", data, ouputs32[0]);
            System.out.println("should be equal to 0x - " + data);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] bytTestTwo = inputs[5].getBytes();
        String dataTwo = BaseConverter.encode32(bytTestTwo);

        try {
            assertEquals("should be equal to 0x66", dataTwo, ouputs32[3]);
            System.out.println("should be equal to 0x66 - " + dataTwo);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        // byte[] bytTestThree = inputs[8].getBytes();
        // String dataThree = BaseConverter.encode32(bytTestThree);

        // System.out.println("should be equal to  - " + dataThree);
        // System.out.println("should be equal to  - " + bytTestThree);
        // System.out.println("should be equal to  - " + ouputs32[8]);

        // try {
        //     assertEquals("should be equal - ", dataThree, ouputs32[5]);
        //     System.out.println("should be equal - " + dataThree);
        // } catch (ArrayComparisonFailure failure) {
        //     System.out.println("Test failed: " + failure.getMessage());
        //     assertFalse("test should not fail:" + failure.getMessage(), true);
        // }
    }

    @Test
    public void testBaseDecoder32() {

        byte[] data = BaseConverter.decode32(ouputs32[0]);
        String decoded = new String(data);

        try {
            assertEquals("should be equal to empty string", decoded, inputs[0]);
            System.out.println("should be equal to 0x - " + data);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        byte[] dataTwo = BaseConverter.decode32(ouputs32[3]);
        String decodedTwo = new String(dataTwo);

        System.out.println("should be equal to  - " + dataTwo);
        System.out.println("should be equal to  - " + decodedTwo);
        System.out.println("should be equal to  - " + ouputs32[3]);

        try {
            assertEquals("should be equal to empty string", decodedTwo, inputs[5]);
            System.out.println("should be equal to 0x - " + data);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        // byte[] dataThree = BaseConverter.decode32(ouputs32[5]);
        // String decodedThree = new String(dataThree);

        // System.out.println("should be equal to  - " + dataThree);
        // System.out.println("should be equal to  - needed value " + decodedThree);
        // // System.out.println("should be equal to  - " + ouputs32[3]);

        // try {
        //     assertEquals("should be equal to empty string", decodedThree, inputs[8]);
        //     System.out.println("should be equal to 0x - " + data);
        // } catch (ArrayComparisonFailure failure) {
        //     System.out.println("Test failed: " + failure.getMessage());
        //     assertFalse("test should not fail:" + failure.getMessage(), true);
        // }

    }

}