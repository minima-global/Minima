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
        System.out.println("Starting test");

        String publicKey = super.minimaTestNode.getPublicKey();

        System.out.println("Public key is " + publicKey);

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = super.minimaTestNode.runCommand("sign data:"+data+" publickey:"+publicKey);

        runBaseTestsOnExpectedArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean)json.get("status"));

        assertFalse((boolean)json.get("pending"));

        String signature = json.get("response").toString();

        System.out.println("Veryfiying the signature");

        output = super.minimaTestNode.runCommand("verify data:"+data+" publickey:"+publicKey + " signature:" + signature);

        System.out.println("Result of the signature verification");

        System.out.println(output.toString());

        json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean)json.get("status"));

        assertFalse((boolean)json.get("pending"));
    }

    @Test
    public void testSignWithInvalidPublicKeyAndDataArgs () throws Exception 
    {
        System.out.println("Starting test");

        String publicKey = "0xC53237461AC2B614CF258E6B4D07A693642EE8F104B14BE6DE09C09370A7CF2E"; //Public key of another separate account

        System.out.println("Public key is " + publicKey);

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        //this should fail
        String output = super.minimaTestNode.runCommand("sign data:"+data+" publickey:"+publicKey);

        runBaseTestsOnExpectedArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean)json.get("status"));

        assertFalse((boolean)json.get("pending"));
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
        System.out.println("status must be false: " + json.get("status"));
        assertFalse((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean)json.get("pending"));
    }

    public void runBaseTestsOnExpectedArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be false: " + json.get("status"));
        assertTrue((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean)json.get("pending"));
    }

}