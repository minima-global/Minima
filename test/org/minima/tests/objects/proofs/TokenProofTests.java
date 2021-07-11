package org.minima.tests.objects.proofs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import static org.junit.Assert.assertThrows;

import org.junit.Test;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.proofs.TokenProof;
import org.minima.utils.json.JSONObject;

public class TokenProofTests {

    @Test
    public void testTokenProof() {
        MiniString name = new MiniString("token-name");
        MiniString script = new MiniString("FFFF");
        MiniData coinId = new MiniData("0xffff");
        MiniNumber twelve = MiniNumber.TWELVE;
        TokenProof t = new TokenProof(coinId, twelve, twelve, name, script);
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

        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("TokenName"), new MiniString("RETURN TRUE"));
            assertEquals("1", tp.getScaledMinimaAmount(new MiniNumber(10000)).toString());
            assertEquals("10", tp.getScaledMinimaAmount(new MiniNumber(100000)).toString());
            assertEquals("100", tp.getScaledMinimaAmount(new MiniNumber(1000000)).toString());

            assertEquals("TokenName", tp.getShowName());
        }
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("{\"name\": \"TokenName\"}"), new MiniString("RETURN TRUE"));
            assertEquals("TokenName", tp.getShowName());
        }
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("{'name': 'TokenName'}"), new MiniString("RETURN TRUE"));
            assertEquals("{'name': 'TokenName'}", tp.getShowName()); // Does not parse name with single quotes
        }
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("{\"name1\": \"TokenName\"}"), new MiniString("RETURN TRUE"));
            assertThrows(Exception.class, () -> {
                assertEquals("{\"name1\": \"TokenName\"}", tp.getShowName()); // Throws NullPointerexception on invalid string
            });
        }

    }

    @Test
    public void testStaticReadAndWriteDataStream() {
        try {
            MiniString name = new MiniString("token-name");
            MiniString script = new MiniString("FFFF");
            MiniData coinId = new MiniData("0xffff");
            MiniNumber twelve = MiniNumber.TWELVE;
            TokenProof t = new TokenProof(coinId, twelve, twelve, name, script);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            t.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            TokenProof.ReadFromStream(dis);
            System.out.println("Proof json values in read and write stream - " + t.toJSON());

            assertNotNull(t);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    @Test
    public void testJSONConversion() {
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("TokenName"), new MiniString("RETURN TRUE"));

            JSONObject json = tp.toJSON();

            assertTrue("JSON object should contain tokenid key", json.containsKey("tokenid"));
            assertTrue("JSON object should contain description key", json.containsKey("description"));
            assertTrue("JSON object should contain icon key", json.containsKey("icon"));
            assertTrue("JSON object should contain proof key", json.containsKey("proof"));
            assertTrue("JSON object should contain total key", json.containsKey("total"));
            assertTrue("JSON object should contain decimals key", json.containsKey("decimals"));
            assertTrue("JSON object should contain script key", json.containsKey("script"));
            assertTrue("JSON object should contain coinid key", json.containsKey("coinid"));
            assertTrue("JSON object should contain totalamount key", json.containsKey("totalamount"));
            assertTrue("JSON object should contain scale key", json.containsKey("scale"));
        }
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("{\"name\": \"TokenName\"}"), new MiniString("RETURN TRUE"));

            JSONObject json = tp.toJSON();

            assertTrue("JSON object should contain tokenid key", json.containsKey("tokenid"));
            assertTrue("JSON object should contain description key", json.containsKey("description"));
            assertTrue("JSON object should contain icon key", json.containsKey("icon"));
            assertTrue("JSON object should contain proof key", json.containsKey("proof"));
            assertTrue("JSON object should contain total key", json.containsKey("total"));
            assertTrue("JSON object should contain decimals key", json.containsKey("decimals"));
            assertTrue("JSON object should contain script key", json.containsKey("script"));
            assertTrue("JSON object should contain coinid key", json.containsKey("coinid"));
            assertTrue("JSON object should contain totalamount key", json.containsKey("totalamount"));
            assertTrue("JSON object should contain scale key", json.containsKey("scale"));
        }
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("{\"name\": \"TokenName\", \"description\": \"Description\", \"icon\": \"Icon\", \"proof\": \"Proof\"}"), new MiniString("RETURN TRUE"));

            JSONObject json = tp.toJSON();

            assertTrue("JSON object should contain tokenid key", json.containsKey("tokenid"));
            assertTrue("JSON object should contain description key", json.containsKey("description"));
            assertTrue("JSON object should contain icon key", json.containsKey("icon"));
            assertTrue("JSON object should contain proof key", json.containsKey("proof"));
            assertTrue("JSON object should contain total key", json.containsKey("total"));
            assertTrue("JSON object should contain decimals key", json.containsKey("decimals"));
            assertTrue("JSON object should contain script key", json.containsKey("script"));
            assertTrue("JSON object should contain coinid key", json.containsKey("coinid"));
            assertTrue("JSON object should contain totalamount key", json.containsKey("totalamount"));
            assertTrue("JSON object should contain scale key", json.containsKey("scale"));
        }
        {
            TokenProof tp = new TokenProof(MiniData.getRandomData(16), MiniNumber.FOUR, new MiniNumber(10000), new MiniString("{'name': 'TokenName'}"), new MiniString("RETURN TRUE"));

            JSONObject json = tp.toJSON();

            assertTrue("JSON object should contain tokenid key", json.containsKey("tokenid"));
            assertTrue("JSON object should contain description key", json.containsKey("description"));
            assertTrue("JSON object should contain icon key", json.containsKey("icon"));
            assertTrue("JSON object should contain proof key", json.containsKey("proof"));
            assertTrue("JSON object should contain total key", json.containsKey("total"));
            assertTrue("JSON object should contain decimals key", json.containsKey("decimals"));
            assertTrue("JSON object should contain script key", json.containsKey("script"));
            assertTrue("JSON object should contain coinid key", json.containsKey("coinid"));
            assertTrue("JSON object should contain totalamount key", json.containsKey("totalamount"));
            assertTrue("JSON object should contain scale key", json.containsKey("scale"));
        }
    }
}
