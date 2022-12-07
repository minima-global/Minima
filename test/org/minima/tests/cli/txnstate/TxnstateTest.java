package org.minima.tests.cli.txnstate;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TxnstateTest extends MinimaCliTest {

    @Test
    public void testTxnstateWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("txnstate");
        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxnstateWithValidArgs() throws Exception {
        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"0\"");

        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testTxnstateWithLongArgs() throws Exception {
        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"123\"");

        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testTxnstateWithNoQoutesAroundValueArgument() throws Exception {
        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:0");

        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testTxnstateWithNonNumericValueArgument() throws Exception {
        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"hello\"");

        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxnstateWithHexidecimalValueArgument() throws Exception {
        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"0xfff\"");

        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testTxnstateWithCapitalisedHexidecimalValueArgument() throws Exception {
        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"0xFFF\"");

        runBaseTestsWithValidArgs(output);
    }

    public void runBaseTestsWithInvalidArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse((boolean) json.get("status"), "status must be false: ");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false: ");
    }

    public void runBaseTestsWithValidArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean) json.get("status"), "status must be true: ");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false: ");
    }

}