package org.minima.tests.cli;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.junit.Test;
import org.junit.After;
import static org.junit.Assert.*;

public class MinimaCliTest {

    public void runBaseTests (String output) throws Exception {
        System.out.println("Printing the output of the command:");
        System.out.println(output);

        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be true: " + json.get("status"));
        assertTrue((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean)json.get("pending"));
    }

}

