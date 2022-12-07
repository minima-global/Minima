package org.minima.tests.cli;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class MinimaCliTest {

    public static MinimaTestNode minimaTestNode;

    @BeforeAll
    public static void initTest() throws Exception {
        //Start up Minima
        minimaTestNode = new MinimaTestNode();
    }

    @AfterAll
    public static void finishTest() throws Exception {
        //Shut her down
        minimaTestNode.killMinima();
    }

    public void runBaseTests(String output) throws Exception {
        System.out.println("Printing the output of the command:");
        System.out.println(output);

        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be true: " + json.get("status"));
        assertTrue((boolean) json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean) json.get("pending"));
    }

}
