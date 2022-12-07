package org.minima.tests.cli.maxcontacts;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;

public class MaxContactsTest extends MinimaCliTest {

    @Test
    public void testMaxContactsWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("maxcontacts");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be false: " + json.get("status"));
        assertFalse((boolean) json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean) json.get("pending"));
    }

}