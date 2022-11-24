package org.minima.tests.cli.txnlist;

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

public class TxnlistTest extends MinimaCliTest {

    @Test
    public void testTxnlistWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txnlist");
        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testTxnlistWithInvalidArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txnlist arg:invalid");
        runBaseTestsWithInvalidArgs(output);        
    }

    @Test
    public void testTxnlistWithOneTransaction () throws Exception 
    {

        String chosenTxId = "mytransaction";

        super.minimaTestNode.runCommand("txncreate id:" + chosenTxId);

        String output = super.minimaTestNode.runCommand("txnlist");

        runBaseTestsWithValidArgs (output);

        JSONObject jsonOutput =  (JSONObject) new JSONParser().parse(output.toString());

        JSONArray jsonArray = (JSONArray) jsonOutput.get("response");

        JSONObject zeroth = (JSONObject) jsonArray.get(0);

        String firstTransactionId = zeroth.get("id").toString();

        assertTrue("The transaction id in the list should match the transaction id specified in txncreate ",firstTransactionId.equals(chosenTxId));

    }

    @Test
    public void testTxnlistWithManyTransactions () throws Exception 
    {

        int count = 0;
        int num_new_transactions = 100;
        String chosenTxIdBase = "mytransaction";

        //add 100 transactions
        while(++count <= num_new_transactions)
        {
            
            super.minimaTestNode.runCommand("txncreate id:" + chosenTxIdBase + Integer.toString(count));
        }        

        String output = super.minimaTestNode.runCommand("txnlist");

        runBaseTestsWithValidArgs (output);

        JSONObject jsonOutput =  (JSONObject) new JSONParser().parse(output.toString());

        JSONArray jsonArray = (JSONArray) jsonOutput.get("response");

        count = 0;

        while(++count <= num_new_transactions)
        {

            JSONObject nth_transaction = (JSONObject) jsonArray.get(count);

            String nthTransactionId = nth_transaction.get("id").toString();

            assertTrue("The transaction id in the list should match the transaction id specified in txncreate ", nthTransactionId.equals(chosenTxIdBase + Integer.toString(count)));
        }
    }
    
    public void runBaseTestsWithValidArgs (String output) throws Exception
    {
        super.runBaseTests(output);
    }

    public void runBaseTestsWithInvalidArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("status must be false: ", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false: ", (boolean)json.get("pending"));
    }

}