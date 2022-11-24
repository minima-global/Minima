package org.minima.tests.cli.txnsign;

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

public class TxnsignTest extends MinimaCliTest {

    @Test
    public void testTxnsignWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txnsign");
        runBaseWithInvalidArgsTests(output);
    }

    @Test
    public void testTxnsignWithValidArgs () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:\"newTransaction\"");

        String publicKey = super.minimaTestNode.getPublicKey();

        String output = super.minimaTestNode.runCommand("txnsign id:\"newTransaction\" publickey:" + publicKey);

        runBaseWithValidArgsTests(output);
    }

    @Test
    public void testTxnsignWithTxnThatDoesntExist () throws Exception
    {
        String publicKey = super.minimaTestNode.getPublicKey();

        String output = super.minimaTestNode.runCommand("txnsign id:\"aTransactionThatDoesntExistYet\" publickey:" + publicKey);

        runBaseWithInvalidArgsTests(output);
    }

    @Test
    public void testTxnsignWithNotMyPublicKey () throws Exception
    {
        String publicKey = "0x405E3EAE797BD0148751EB66D356485AA1FD7AD6D962AF439AA3D739C0A8CC8E"; //not my public key

        super.minimaTestNode.runCommand("txncreate id:\"newTransaction\"");

        String output = super.minimaTestNode.runCommand("txnsign id:\"newTransaction\" publickey:" + publicKey);

        runBaseWithInvalidArgsTests(output);
    }

    public void runBaseWithInvalidArgsTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("response status should be false", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("response pending should be false", (boolean)json.get("pending"));
    }

    public void runBaseWithValidArgsTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue("response status should be true", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("response pending should be false", (boolean)json.get("pending"));
    }

}