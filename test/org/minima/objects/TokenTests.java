package org.minima.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.Token;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;

public class TokenTests {

    @Test
    public void testTokenProof() {
        MiniString name = new MiniString("token-name");
        MiniString script = new MiniString("FFFF");
        MiniData coinId = new MiniData("0xffff");
        MiniNumber twelve = MiniNumber.TWELVE;
        Token t = new Token(coinId, twelve, twelve, name, script);
        assertNotNull(t);
        assertEquals("Should be equal ", twelve, t.getAmount());
        System.out.println("amount  value " + t.getAmount());
        assertEquals("Should be equal ", name, t.getName());
        System.out.println("name  value " + t.getName());
        System.out.println("script value " + script + " " + t.getTokenScript());
        // assertTrue("should be equal", t.getTokenScript().equals(script));
        // assertEquals("Should be equal ",script, t.getTokenScript());
        System.out.println("tokenscript  value " + t.getTokenScript());
        System.out.println("coin id  value " + t.getCoinID());
        System.out.println("token id  value " + t.getTokenID());
        assertEquals("Should be equal ", twelve, t.getScale());
        System.out.println("scale value " + t.getScale());
//        System.out.println("scale factor  value " + t.getScaleFactor());
        System.out.println("Json  value " + t.toJSON());

    }

    @Test
    public void testStaticReadAndWriteDataStream() {
        try {
            MiniString name = new MiniString("token-name");
            MiniString script = new MiniString("FFFF");
            MiniData coinId = new MiniData("0xffff");
            MiniNumber twelve = MiniNumber.TWELVE;
            Token t = new Token(coinId, twelve, twelve, name, script);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            t.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            Token.ReadFromStream(dis);
            System.out.println("Proof json values in read and write stream - " + t.toJSON());

            assertNotNull(t);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }
}
