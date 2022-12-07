package org.minima.tests.cli.txndelete;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TxndeleteTest extends MinimaCliTest {

    @Test
    public void testTxndeleteWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("txndelete");
        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxndeleteWithValidArgs() throws Exception {

        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txndelete id:myTransaction");

        runBaseTestsWithValidArgs(output);

    }

    @Test
    public void testTxndeleteWithValidArgsAndQuotes() throws Exception {

        minimaTestNode.runCommand("txncreate id:\"myTransaction\"");

        String output = minimaTestNode.runCommand("txndelete id:\"myTransaction\"");

        runBaseTestsWithValidArgs(output);

    }

    @Test
    public void testTxndeleteTxThatDoesNotExist() throws Exception {

        minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = minimaTestNode.runCommand("txndelete id:notMyTransaction");

        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxndeleteTryOtherTxCommandsOnDeletedTransaction() throws Exception {

        minimaTestNode.runCommand("txncreate id:myTransaction");

        minimaTestNode.runCommand("txndelete id:myTransaction");

        String output = minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"this shouldnt work\"");

        runBaseTestsWithInvalidArgs(output);
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

        System.out.println("The response of the function was");
        System.out.println(json.get("response").toString());
        System.out.println(json.get("response").toString() == "Deleted");

        //Response should be Deleted
        assertTrue(json.get("response").toString().equals("Deleted"), "response must be \"Deleted\": ");
    }

}