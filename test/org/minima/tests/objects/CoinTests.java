package org.minima.tests.objects;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

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

import org.junit.Test;

public class CoinTests {

    @Test
    public void testCoin() {
        MiniData md = Coin.COINID_OUTPUT;
        MiniData md3 = Coin.TOKENID_CREATE;
        MiniData md2 = Coin.MINIMA_TOKENID;
        MiniData coinId = new MiniData("#fff");
        MiniData coinId2 = new MiniData("#f0f0");
        MiniData coinAddress = new MiniData("#ffffffff");
        MiniNumber two = MiniNumber.TWO;
        MiniNumber twelve = MiniNumber.TWELVE;
        MiniData tokenId = new MiniData("123");
        // System.out.println("Minidata values coinid output - " + md);
        // System.out.println("Minidata values tokenid create output - " + md3);
        // System.out.println("Minidata values minima token id output - " + md2);

        Coin c = new Coin(coinId, coinAddress, two, tokenId);
        // System.out.println("Coin created values - " + c.toJSON());
        assertFalse("Floating should be false", c.isFloating());
        c.setFloating(true);
        assertTrue("Floating should be true", c.isFloating());
        assertFalse("Remainder should be false", c.isRemainder());
        c.setRemainder(true);
        assertTrue("Remainder should be true", c.isRemainder());
        // System.out.println("Coin id value - " + c.mCoinID);
        c.resetCoinID(coinId2);
        // System.out.println("Coin id value after reset - " + c.mCoinID);
        // System.out.println("Amount value before reset - " + c.mAmount);
        c.resetAmount(twelve);
        // System.out.println("Amount value before reset - " + c.mAmount);
        assertTrue("Token id should equal", c.getTokenID().equals(tokenId));
        assertTrue("Amount should equal", c.getAmount().equals(twelve));
        assertEquals("Address should equal", c.getAddress(), coinAddress);
        // System.out.println("Verify this value*** - " + c.getCoinID());
        // System.out.println("Coin to string value - " + c.toString());

        assertTrue("c coin id should now be set to coinId2", c.getCoinID().isEqual(coinId2)); // Could be bug in code #ffff should be
        // replaced by #f0f0 instead value #FFF0F0 could be related to floating point?
    }

    @Test
    public void testReadAndWriteDataStream() {
        try {
            MiniData coinId = new MiniData("#fff");
            MiniData coinAddress = new MiniData("#ffffffff");
            MiniNumber twelve = MiniNumber.TWELVE;
            MiniData tokenId = new MiniData("123");

            Coin c = new Coin(coinId, coinAddress, twelve, tokenId);
            c.setRemainder(true);
            c.setFloating(true);
            // System.out.println("coin  value before write " + c.toString());

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            c.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            c.readDataStream(dis);

            // System.out.println("coin  value after read" + c.toString());

            assertNotNull(c);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    @Test
    public void testStaticReadAndWriteDataStream() {
        try {
            MiniData coinId = new MiniData("#fff");
            MiniData coinAddress = new MiniData("#ffffffff");
            MiniNumber twelve = MiniNumber.TWELVE;
            MiniData tokenId = new MiniData("123");

            Coin c = new Coin(coinId, coinAddress, twelve, tokenId);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            c.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            Coin.ReadFromStream(dis);

            assertNotNull(c);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }
}
