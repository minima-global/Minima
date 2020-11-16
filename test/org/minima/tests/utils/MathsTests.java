package org.minima.tests.utils;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.utils.Maths;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.math.BigDecimal;
import java.math.BigInteger;

public class MathsTests {

    @Test
    public void testGeneralMathsFunctions() {
        BigDecimal bigDecimal = Maths.BD_TWO;
        assertNotNull(bigDecimal);
        System.out.println("should be create a bid decimal with value 2 ");
        Maths mathObj = new Maths();
        assertNotNull(mathObj);
        System.out.println("creates new instance of math object");
    }

    @Test
    public void testLog2() {

        double testValue = Maths.log2(2);
        double testResult = 1.00;

        try {
            assertEquals(testValue, testResult, 0);
            System.out.println("should be equal to 1.0 - " + testValue);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        double testValueTwo = Maths.log2(4);
        double testResultTwo = 2.00;

        try {
            assertEquals(testValueTwo, testResultTwo, 0);
            System.out.println("should be equal to 2.0 - " + testValueTwo);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        double testValueThree = Maths.log2(8);
        double testResultThree = 3.00;

        try {
            assertEquals(testValueThree, testResultThree, 0);
            System.out.println("should be equal to 3.0 - " + testValueThree);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        double testValueFour = Maths.log2(256);
        double testResultFour = 8.00;

        try {
            assertEquals(testValueFour, testResultFour, 0);
            System.out.println("should be equal to 8.0 - " + testValueFour);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

    }

    @Test
    public void testLog2BI() {
        BigInteger testValue = new BigInteger("2");
        double testResult = Maths.log2BI(testValue);
        double exxpectedResult = 1;

        System.out.println("should be equal to - 1.0 " + testResult);

        try {
            assertEquals(testResult, exxpectedResult, 0);
            System.out.println("should be equal to 1 - " + testResult);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        BigInteger testValueTwo = new BigInteger("4");
        double testResultTwo = Maths.log2BI(testValueTwo);
        double exxpectedResultTwo = 2;

        System.out.println("should be equal to - 2.0 " + testResultTwo);

        try {
            assertEquals(testResultTwo, exxpectedResultTwo, 0);
            System.out.println("should be equal to 2.0 - " + testResultTwo);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        BigInteger testValueThree = new BigInteger("8");
        double testResultThree = Maths.log2BI(testValueThree);
        double exxpectedResultThree = 3;

        System.out.println("should be equal to - 3.0 " + testResultThree);

        try {
            assertEquals(testResultThree, exxpectedResultThree, 0);
            System.out.println("should be equal to 3.0 - " + testResultThree);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        BigInteger testValueFour = new BigInteger("256");
        double testResultFour = Maths.log2BI(testValueFour);
        double exxpectedResultFour = 8;

        System.out.println("should be equal to - 8.0 " + testResultFour);

        try {
            assertEquals(testResultFour, exxpectedResultFour, 0);
            System.out.println("should be equal to 8.0 - " + testResultFour);
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        // BigInteger testValueFive = new BigInteger("10002022048");
        // double testResultFive = Maths.log2BI(testValueFive);
        // double exxpectedResultFive = 33.21957263924622;
        // System.out.println("should be equal to - 10.0 " + testResultFive);
        // try {
        //     assertEquals(testResultFive, exxpectedResultFive, 0);
        //     System.out.println("should be equal to 10.0 - " + testResultFive);
        // } catch (ArrayComparisonFailure failure) {
        //     System.out.println("Test failed: " + failure.getMessage());
        //     assertFalse("test should not fail:" + failure.getMessage(), true);
        // }
    }

    @Test
    public void testConvertMilliToTime() {

        String expectedTime = "151 Years 1 Months 0 Weeks 0 Days 15 Hours 46 Minutes 5 Seconds 482 Milliseconds";
        try {
            assertEquals("should be equal to  - ", Maths.ConvertMilliToTime(4764584765482L), expectedTime);
            System.out.println("should be equal to  - " + Maths.ConvertMilliToTime(4764584765482L));
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }

        String expectedTimeTwo = "213 Years 8 Months 0 Weeks 0 Days 8 Hours 19 Minutes 59 Seconds 444 Milliseconds";
        try {
            assertEquals("should be equal to  - ", Maths.ConvertMilliToTime(6737933999444L), expectedTimeTwo);
            System.out.println("should be equal to  - " + Maths.ConvertMilliToTime(6737933999444L));
        } catch (ArrayComparisonFailure failure) {
            System.out.println("Test failed: " + failure.getMessage());
            assertFalse("test should not fail:" + failure.getMessage(), true);
        }
    }

}
