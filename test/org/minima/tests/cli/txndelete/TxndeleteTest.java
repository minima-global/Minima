package org.minima.tests.cli.txndelete;

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

public class TxndeleteTest extends MinimaCliTest {

    @Test
    public void testTxndeleteWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txndelete");
        runBaseTestsWithInvalidArgs(output);        
    }

    @Test
    public void testTxndeleteWithValidArgs () throws Exception
    {

        super.minimaTestNode.runCommand("txncreate id:myTransaction");
        
        String output = super.minimaTestNode.runCommand("txndelete id:myTransaction");
        
        runBaseTestsWithValidArgs(output);        

    }

    @Test
    public void testTxndeleteWithValidArgsAndQuotes () throws Exception
    {

        super.minimaTestNode.runCommand("txncreate id:\"myTransaction\"");
        
        String output = super.minimaTestNode.runCommand("txndelete id:\"myTransaction\"");
        
        runBaseTestsWithValidArgs(output);        
        
    }

    @Test
    public void testTxndeleteTxThatDoesNotExist () throws Exception
    {

        super.minimaTestNode.runCommand("txncreate id:myTransaction");
        
        String output = super.minimaTestNode.runCommand("txndelete id:notMyTransaction");
        
        runBaseTestsWithInvalidArgs(output);        
    }

    @Test
    public void testTxndeleteTryOtherTxCommandsOnDeletedTransaction () throws Exception
    {

        super.minimaTestNode.runCommand("txncreate id:myTransaction");
        
        super.minimaTestNode.runCommand("txndelete id:myTransaction");
        
        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"this shouldnt work\"");

        runBaseTestsWithInvalidArgs(output);        
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

    public void runBaseTestsWithValidArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

       //status of the cmd request must be true
       assertTrue("status must be true: ", (boolean)json.get("status"));

       //cmd response pending should be false
       assertFalse("pending must be false: ", (boolean)json.get("pending"));

        System.out.println("The response of the function was");
        System.out.println(json.get("response").toString());
        System.out.println(json.get("response").toString() == "Deleted");

       //Response should be Deleted
       assertTrue("response must be \"Deleted\": ", json.get("response").toString().equals("Deleted"));
    }

}