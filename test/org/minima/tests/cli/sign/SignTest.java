package org.minima.tests.cli.sign;

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

public class SignTest extends MinimaCliTest {

    @Test
    public void testSignWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("sign");

        runBaseTests(output);     
    }

    @Test
    public void testSignWithPublicKeyAndDataArgs () throws Exception 
    {
        String publicKey = super.minimaTestNode.getPublicKey();

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = super.minimaTestNode.runCommand("sign data:"+data+" publickey:"+publicKey);

        runBaseTestsOnExpectedArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue("status must be true", (boolean)json.get("status"));

        assertFalse("pending must be true", (boolean)json.get("pending"));

        String signature = json.get("response").toString();

        output = super.minimaTestNode.runCommand("verify data:"+data+" publickey:"+publicKey + " signature:" + signature);

        json = (JSONObject) new JSONParser().parse(output);

        assertTrue("status must be true",(boolean)json.get("status"));

        assertFalse("pending must be false",(boolean)json.get("pending"));
    }

    @Test
    public void testSignWithInvalidPublicKeyAndDataArgs () throws Exception 
    {
        String publicKey = "0xC53237461AC2B614CF258E6B4D07A693642EE8F104B14BE6DE09C09370A7CF2E"; //Public key of another separate account

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        //this should fail
        String output = super.minimaTestNode.runCommand("sign data:"+data+" publickey:"+publicKey);

        runBaseTestsOnExpectedArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue("status must be true", (boolean)json.get("status"));

        assertFalse("pending must be false", (boolean)json.get("pending"));
    }

    @Test
    public void testSignWithOnlyPublicKeyArgs () throws Exception 
    {
        String output = super.minimaTestNode.runCommand("sign");

        runBaseTests(output);     
    }

    @Test
    public void testSignWithOnlyDataArgs () throws Exception 
    {
        String output = super.minimaTestNode.runCommand("sign");

        runBaseTests(output);       
        
    }

    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("status must be false", (boolean)json.get("status"));

        assertFalse("pending must be false", (boolean)json.get("pending"));
    }

    public void runBaseTestsOnExpectedArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue("status must be true", (boolean)json.get("status"));

        assertFalse("pending must be false", (boolean)json.get("pending"));
    }

}