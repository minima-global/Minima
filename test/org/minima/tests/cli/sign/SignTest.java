package org.minima.tests.cli.sign;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SignTest extends MinimaCliTest {

    @Test
    public void testSignWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("sign");

        runBaseTests(output);
    }

    @Test
    public void testSignWithPublicKeyAndDataArgs() throws Exception {
        String publicKey = minimaTestNode.getPublicKey();

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = minimaTestNode.runCommand("sign data:" + data + " publickey:" + publicKey);

        runBaseTestsOnExpectedArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean) json.get("status"), "status must be true");

        assertFalse((boolean) json.get("pending"), "pending must be true");

        String signature = json.get("response").toString();

        output = minimaTestNode.runCommand("verify data:" + data + " publickey:" + publicKey + " signature:" + signature);

        json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean) json.get("status"), "status must be true");

        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

    @Test
    public void testSignWithInvalidPublicKeyAndDataArgs() throws Exception {
        String publicKey = "0xC53237461AC2B614CF258E6B4D07A693642EE8F104B14BE6DE09C09370A7CF2E"; //Public key of another separate account

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        //this should fail
        String output = minimaTestNode.runCommand("sign data:" + data + " publickey:" + publicKey);

        runBaseTestsOnExpectedArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean) json.get("status"), "status must be true");

        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

    @Test
    public void testSignWithOnlyPublicKeyArgs() throws Exception {
        String output = minimaTestNode.runCommand("sign");

        runBaseTests(output);
    }

    @Test
    public void testSignWithOnlyDataArgs() throws Exception {
        String output = minimaTestNode.runCommand("sign");

        runBaseTests(output);

    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse((boolean) json.get("status"), "status must be false");

        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

    public void runBaseTestsOnExpectedArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean) json.get("status"), "status must be true");

        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

}