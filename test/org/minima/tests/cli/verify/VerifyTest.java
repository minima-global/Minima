package org.minima.tests.cli.verify;

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

public class VerifyTest extends MinimaCliTest {

    @Test
    public void testVerifyWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("verify");

        runBaseTests(output);        
    }

    @Test 
    public void testVerifyWithValidArgs () throws Exception 
    {
        String publicKey = super.minimaTestNode.getPublicKey();

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = super.minimaTestNode.runCommand("sign data:"+data+" publickey:"+publicKey);

        runBaseTestsWithValidArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue("status must be true", (boolean)json.get("status"));

        assertFalse("status must be false", (boolean)json.get("pending"));

        String signature = json.get("response").toString();

        output = super.minimaTestNode.runCommand("verify data:"+data+" publickey:"+publicKey + " signature:" + signature);

        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testVerifyWithInvalidArgs () throws Exception 
    {
        String publicKey = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //not my public key

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = super.minimaTestNode.runCommand("sign data:"+data+" publickey:"+publicKey);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertFalse("status must be false", (boolean)json.get("status"));

        assertFalse("pending must be true", (boolean)json.get("pending"));

        String signature = json.get("response").toString();

        output = super.minimaTestNode.runCommand("verify data:"+data+" publickey:"+publicKey + " signature:" + signature);

        runBaseTestsWithInvalidArgs(output);
    }
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be false
        assertFalse("status must be false", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false",(boolean)json.get("pending"));
    }

    public void runBaseTestsWithValidArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue("status must be true", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false", (boolean)json.get("pending"));
    }

    public void runBaseTestsWithInvalidArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("status must be false", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false", (boolean)json.get("pending"));
    }
}