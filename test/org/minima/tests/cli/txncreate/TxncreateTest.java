package org.minima.tests.cli.txncreate;

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

public class TxncreateTest extends MinimaCliTest {

    @Test
    public void testTxncreateWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txncreate");
        runBaseTests(output);        
    }

    @Test
    public void testTxncreateWithValidArgs () throws Exception 
    {
        String output = super.minimaTestNode.runCommand("txncreate id:\"myTransaction\"");

        runBaseTestsWithValidArgs (output);
    }

    @Test
    public void testTxncreateTwiceWithSameID () throws Exception 
    {
        super.minimaTestNode.runCommand("txncreate id:\"myTransaction\"");
        
        String output = super.minimaTestNode.runCommand("txncreate id:\"myTransaction\"");

        runBaseTestsWithInvalidArgs (output);
    }
    
    @Test
    public void testTxncreateWithNoDataInIDArgument () throws Exception 
    {        
        String output = super.minimaTestNode.runCommand("txncreate id:\"\"");

        runBaseTestsWithInvalidArgs (output);
    }

    @Test
    public void testTxncreateWithLongIDArgument () throws Exception 
    {        
        String output = super.minimaTestNode.runCommand("txncreate id:\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\"");

        runBaseTestsWithValidArgs (output);
    }  
    
    public void testTxncreateWithApostropheAsIDArgument () throws Exception 
    {        
        String output = super.minimaTestNode.runCommand("txncreate id:\"'\"");

        runBaseTestsWithInvalidArgs (output);
    }
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("response status should be false", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("response pending should be false", (boolean)json.get("pending"));
    }

    public void runBaseTestsWithValidArgs (String output) throws Exception 
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be true: " + json.get("status"));
        assertTrue("response status should be true", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("response pending should be false", (boolean)json.get("pending"));
    }

    public void runBaseTestsWithInvalidArgs (String output) throws Exception 
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        System.out.println("THE OUTPUT OF THE TEST ");
        System.out.println(output);

        //status of the cmd request must be true
        System.out.println("status must be false: " + json.get("status"));
        assertFalse((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean)json.get("pending"));
    }

}