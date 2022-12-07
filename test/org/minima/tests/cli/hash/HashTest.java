package org.minima.tests.cli.hash;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import java.io.FileReader;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class HashTest extends MinimaCliTest {

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse((boolean) json.get("status"), "status must be false: ");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false:");
    }

    @Test
    public void testHashWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("hash");

        runBaseTests(output);
    }

    @Test
    public void runFuzzyTest() throws Exception {

        JSONParser parser = new JSONParser();
        JSONArray arr = (JSONArray) parser.parse(new FileReader("test/org/minima/tests/naughtyStrings/blns.json"));

        System.out.println("Testing fuzzy hashing: ");
        for (Object o : arr) {

            String output = minimaTestNode.runCommand("hash data:'" + o + "'");

            JSONObject json = (JSONObject) new JSONParser().parse(output);

            boolean status = (boolean) json.get("status");

            assertThat(status).as("check [%s]'s hash", o).isTrue();
        }
    }

    @Test
    public void runKeccakTest() throws Exception {
        String output = minimaTestNode.runCommand("hash data:thisisadatastring12345 type:keccak");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject InnerResponseJSON = (JSONObject) json.get("response");
        String hashResponse = InnerResponseJSON.getString("hash");
        String expectedResult = "0xFD1FFC2F69003EE91D43AEF271B7FBA023121DCA7937334E6EA8CE7059BB756F";

        assertTrue(hashResponse.equals(expectedResult), "hash response result is not as expected: ");
    }

    @Test
    public void runSha2Test() throws Exception {
        String output = minimaTestNode.runCommand("hash data:thisisadatastring12345 type:sha2");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject InnerResponseJSON = (JSONObject) json.get("response");
        String hashResponse = InnerResponseJSON.getString("hash");
        String expectedResult = "0xB20E583B50A4E5E5CD3F4D65D2E0C783F8D8BAE0EBB78888BB5A977487C18064";

        assertTrue(hashResponse.equals(expectedResult), "hash response result is not as expected: ");
    }

    @Test
    public void runSha3Test() throws Exception {
        String output = minimaTestNode.runCommand("hash data:thisisadatastring12345 type:sha3");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject InnerResponseJSON = (JSONObject) json.get("response");
        String hashResponse = InnerResponseJSON.getString("hash");
        String expectedResult = "0xC11DC39488565959A2A427C109415E41BEAA3816D3BB6B90FCF04E8EA09ED437";

        assertTrue(hashResponse.equals(expectedResult), "hash response result is not as expected: ");
    }

    @Test
    public void runInvalidTypeTest() throws Exception {
        String output = minimaTestNode.runCommand("hash data:thisisadatastring12345 type:sha256");

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "status must be false");
    }
}