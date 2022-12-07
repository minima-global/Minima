package org.minima.tests.cli.verify;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class VerifyTest extends MinimaCliTest {

    @Test
    public void testVerifyWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("verify");

        runBaseTests(output);
    }

    @Test
    public void testVerifyWithValidArgs() throws Exception {
        String publicKey = minimaTestNode.getPublicKey();

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = minimaTestNode.runCommand("sign data:" + data + " publickey:" + publicKey);

        runBaseTestsWithValidArgs(output);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertTrue((boolean) json.get("status"), "status must be true");

        assertFalse((boolean) json.get("pending"), "status must be false");

        String signature = json.get("response").toString();

        output = minimaTestNode.runCommand("verify data:" + data + " publickey:" + publicKey + " signature:" + signature);

        runBaseTestsWithValidArgs(output);
    }

    @Test
    public void testVerifyWithInvalidArgs() throws Exception {
        String publicKey = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //not my public key

        String data = "0x26DA9AE7D32A5702FB18D3D3034EF3EE50A8BA1F2DE0D280B0F2BB458B91A5F5"; //random data

        String output = minimaTestNode.runCommand("sign data:" + data + " publickey:" + publicKey);

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "status must be false");

        assertFalse((boolean) json.get("pending"), "pending must be true");

        String signature = json.get("response").toString();

        output = minimaTestNode.runCommand("verify data:" + data + " publickey:" + publicKey + " signature:" + signature);

        runBaseTestsWithInvalidArgs(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be false
        assertFalse((boolean) json.get("status"), "status must be false");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

    public void runBaseTestsWithValidArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean) json.get("status"), "status must be true");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

    public void runBaseTestsWithInvalidArgs(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse((boolean) json.get("status"), "status must be false");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "pending must be false");
    }
}