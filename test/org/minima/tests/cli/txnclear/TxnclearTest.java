package org.minima.tests.cli.txnclear;


import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.*;

public class TxnclearTest extends MinimaCliTest {

    @Test
    public void testTxnclearWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("txnclear");
        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxnclearWithIdArg() throws Exception {
        minimaTestNode.runCommand("txncreate id:mytx");

        String output = minimaTestNode.runCommand("txnclear id:mytx");

        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testTxnclearWithBadIdArg() throws Exception {
        minimaTestNode.runCommand("txncreate id:mytx");

        String output = minimaTestNode.runCommand("txnclear id:notmytx");

        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxnclearTwiceOnTheSameTx() throws Exception {
        minimaTestNode.runCommand("txncreate id:mytx");

        minimaTestNode.runCommand("txnclear id:mytx");

        String output = minimaTestNode.runCommand("txnclear id:mytx");

        runBaseTestsWithInvalidArgs(output);
    }

    public void runBaseTestsWithValidArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean) json.get("status"), "status must be true: ");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false: ");
    }

    public void runBaseTestsWithInvalidArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse((boolean) json.get("status"), "status must be false: ");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false: ");
    }

}