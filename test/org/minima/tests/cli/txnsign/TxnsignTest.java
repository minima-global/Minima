package org.minima.tests.cli.txnsign;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TxnsignTest extends MinimaCliTest {

    @Test
    public void testTxnsignWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("txnsign");
        runBaseWithInvalidArgsTests(output);
    }

    @Test
    public void testTxnsignWithValidArgs() throws Exception {
        minimaTestNode.runCommand("txncreate id:\"newTransaction\"");

        String publicKey = minimaTestNode.getPublicKey();

        String output = minimaTestNode.runCommand("txnsign id:\"newTransaction\" publickey:" + publicKey);

        runBaseWithValidArgsTests(output);
    }

    @Test
    public void testTxnsignWithTxnThatDoesntExist() throws Exception {
        String publicKey = minimaTestNode.getPublicKey();

        String output = minimaTestNode.runCommand("txnsign id:\"aTransactionThatDoesntExistYet\" publickey:" + publicKey);

        runBaseWithInvalidArgsTests(output);
    }

    @Test
    public void testTxnsignWithNotMyPublicKey() throws Exception {
        String publicKey = "0x405E3EAE797BD0148751EB66D356485AA1FD7AD6D962AF439AA3D739C0A8CC8E"; //not my public key

        minimaTestNode.runCommand("txncreate id:\"newTransaction\"");

        String output = minimaTestNode.runCommand("txnsign id:\"newTransaction\" publickey:" + publicKey);

        runBaseWithInvalidArgsTests(output);
    }

    public void runBaseWithInvalidArgsTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse((boolean) json.get("status"), "response status should be false");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "response pending should be false");
    }

    public void runBaseWithValidArgsTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean) json.get("status"), "response status should be true");

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"), "response pending should be false");
    }

}