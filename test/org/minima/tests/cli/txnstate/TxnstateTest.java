package org.minima.tests.cli.txnstate;

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

public class TxnstateTest extends MinimaCliTest {

    @Test
    public void testTxnstateWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txnstate");
        runBaseTestsWithInvalidArgs(output);        
    }

    @Test
    public void testTxnstateWithValidArgs () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"0\"");
        
        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testTxnstateWithLongArgs () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"123\"");
        
        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testTxnstateWithNoQoutesAroundValueArgument () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:0");
        
        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testTxnstateWithNonNumericValueArgument () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"hello\"");
        
        runBaseTestsWithInvalidArgs(output);     
    }

    @Test
    public void testTxnstateWithHexidecimalValueArgument () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"0xfff\"");
        
        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testTxnstateWithCapitalisedHexidecimalValueArgument () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:myTransaction");

        String output = super.minimaTestNode.runCommand("txnstate id:myTransaction port:0 value:\"0xFFF\"");
        
        runBaseTestsWithValidArgs(output);        
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
    }

}