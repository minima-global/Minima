package org.minima.tests.cli.hash;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;
import static org.junit.Assert.*;

import org.minima.system.commands.CommandException;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.system.Main;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;

public class HashTest extends MinimaCliTest {
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("status must be false: ", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false:",(boolean)json.get("pending"));
    }

    @Test
    public void testHashWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("hash");

        runBaseTests(output);      
    }

    @Test
    public void runFuzzyTest() throws Exception {

        JSONParser parser = new JSONParser();
        JSONArray arr = (JSONArray) parser.parse(new FileReader("test/org/minima/tests/naughtyStrings/blns.json"));

        System.out.println("Testing fuzzy hashing: ");
        for(Object o: arr){        

            String output = super.minimaTestNode.runCommand("hash data:'"+o+"'");

            JSONObject json = (JSONObject) new JSONParser().parse(output);
            
            assertTrue("status must be true: "(boolean)json.get("status"));
        }
    }

    @Test
    public void runKeccakTest() throws Exception {
        String output = super.minimaTestNode.runCommand("hash data:thisisadatastring12345 type:keccak");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject InnerResponseJSON = (JSONObject) json.get("response");
        String hashResponse = InnerResponseJSON.getString("hash");
        String expectedResult = "0xFD1FFC2F69003EE91D43AEF271B7FBA023121DCA7937334E6EA8CE7059BB756F";

        assertTrue("hash response result is not as expected: "hashResponse.equals(expectedResult));
    }

    @Test
    public void runSha2Test() throws Exception {
        String output = super.minimaTestNode.runCommand("hash data:thisisadatastring12345 type:sha2");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject InnerResponseJSON = (JSONObject) json.get("response");
        String hashResponse = InnerResponseJSON.getString("hash");
        String expectedResult = "0xB20E583B50A4E5E5CD3F4D65D2E0C783F8D8BAE0EBB78888BB5A977487C18064";

        assertTrue("hash response result is not as expected: ", hashResponse.equals(expectedResult));
    }

    @Test
    public void runSha3Test() throws Exception {
        String output = super.minimaTestNode.runCommand("hash data:thisisadatastring12345 type:sha3");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject InnerResponseJSON = (JSONObject) json.get("response");
        String hashResponse = InnerResponseJSON.getString("hash");
        String expectedResult = "0xC11DC39488565959A2A427C109415E41BEAA3816D3BB6B90FCF04E8EA09ED437";

        assertTrue("hash response result is not as expected: ", hashResponse.equals(expectedResult));
    }

    @Test
    public void runInvalidTypeTest() throws Exception {
        String output = super.minimaTestNode.runCommand("hash data:thisisadatastring12345 type:sha256");

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertFalse("status must be false", (boolean)json.get("status"));
    }    
}