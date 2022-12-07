package org.minima.tests.cli.txnlist;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TxnlistTest extends MinimaCliTest {

    @Test
    public void testTxnlistWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("txnlist");
        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testTxnlistWithInvalidArgs() throws Exception {
        String output = minimaTestNode.runCommand("txnlist arg:invalid");
        runBaseTestsWithInvalidArgs(output);
    }

    @Test
    public void testTxnlistWithOneTransaction() throws Exception {

        String chosenTxId = "mytransaction";

        minimaTestNode.runCommand("txncreate id:" + chosenTxId);

        String output = minimaTestNode.runCommand("txnlist");

        runBaseTestsWithValidArgs(output);

        JSONObject jsonOutput = (JSONObject) new JSONParser().parse(output.toString());

        JSONArray jsonArray = (JSONArray) jsonOutput.get("response");

        JSONObject zeroth = (JSONObject) jsonArray.get(0);

        String firstTransactionId = zeroth.get("id").toString();

        assertTrue(firstTransactionId.equals(chosenTxId), "The transaction id in the list should match the transaction id specified in txncreate ");

    }

    @Test
    public void testTxnlistWithManyTransactions() throws Exception {

        int count = 0;
        int num_new_transactions = 100;
        String chosenTxIdBase = "mytransaction";

        //add 100 transactions
        while (++count <= num_new_transactions) {

            minimaTestNode.runCommand("txncreate id:" + chosenTxIdBase + Integer.toString(count));
        }

        String output = minimaTestNode.runCommand("txnlist");

        runBaseTestsWithValidArgs(output);

        JSONObject jsonOutput = (JSONObject) new JSONParser().parse(output.toString());

        JSONArray jsonArray = (JSONArray) jsonOutput.get("response");

        count = 0;

        while (++count <= num_new_transactions) {

            JSONObject nth_transaction = (JSONObject) jsonArray.get(count);

            String nthTransactionId = nth_transaction.get("id").toString();

            assertTrue(nthTransactionId.equals(chosenTxIdBase + Integer.toString(count)), "The transaction id in the list should match the transaction id specified in txncreate ");
        }
    }

    public void runBaseTestsWithValidArgs(String output) throws Exception {
        super.runBaseTests(output);
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