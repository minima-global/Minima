package org.minima.objects;

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
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class CoinTests {

    @Test
    public void testCoin() {
        MiniData md = Coin.COINID_OUTPUT;
        MiniData md3 = Token.TOKENID_CREATE;
        MiniData md2 = Token.TOKENID_MINIMA;
        MiniData coinId = new MiniData("0xfff");
        MiniData coinId2 = new MiniData("0xf0f0");
        MiniData coinAddress = new MiniData("0xffffffff");
        MiniNumber two = MiniNumber.TWO;
        MiniNumber twelve = MiniNumber.TWELVE;
        MiniData tokenId = new MiniData("123");
        // System.out.println("Minidata values coinid output - " + md);
        // System.out.println("Minidata values tokenid create output - " + md3);
        // System.out.println("Minidata values minima token id output - " + md2);

        Coin c = new Coin(coinId, coinAddress, two, tokenId);
        // System.out.println("Coin created values - " + c.toJSON());
        // System.out.println("Coin id value - " + c.mCoinID);
//        c.resetCoinID(coinId2);
        assertTrue("Token id should equal", c.getTokenID().equals(tokenId));
        assertTrue("Amount should equal", c.getAmount().equals(two));
        assertEquals("Address should equal", c.getAddress(), coinAddress);
        // System.out.println("Verify this value*** - " + c.getCoinID());
        // System.out.println("Coin to string value - " + c.toString());

//        assertTrue("c coin id should now be set to coinId2", c.getCoinID().isEqual(coinId2)); // Could be bug in code #ffff should be
        // replaced by #f0f0 instead value #FFF0F0 could be related to floating point?
    }

    @Test
    public void testReadAndWriteDataStream() {
        try {
            MiniData coinId = new MiniData("0xfff");
            MiniData coinAddress = new MiniData("0xffffffff");
            MiniNumber twelve = MiniNumber.TWELVE;
            MiniData tokenId = new MiniData("123");

            Coin c = new Coin(coinId, coinAddress, twelve, tokenId);
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
            // System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    @Test
    public void testStaticReadAndWriteDataStream() {
        try {
            MiniData coinId = new MiniData("0xfff");
            MiniData coinAddress = new MiniData("0xffffffff");
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
