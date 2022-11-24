package org.minima.tests.cli.txnclear;

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

public class TxnclearTest extends MinimaCliTest {

    @Test
    public void testTxnclearWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("txnclear");
        runBaseTestsWithInvalidArgs(output);        
    }

    @Test
    public void testTxnclearWithIdArg () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:mytx");

        String output = super.minimaTestNode.runCommand("txnclear id:mytx");

        runBaseTestsWithValidArgs(output);        
    }

    @Test
    public void testTxnclearWithBadIdArg () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:mytx");

        String output = super.minimaTestNode.runCommand("txnclear id:notmytx");
        
        runBaseTestsWithInvalidArgs(output);        
    }

    @Test
    public void testTxnclearTwiceOnTheSameTx () throws Exception
    {
        super.minimaTestNode.runCommand("txncreate id:mytx");

        super.minimaTestNode.runCommand("txnclear id:mytx");

        String output = super.minimaTestNode.runCommand("txnclear id:mytx");
        
        runBaseTestsWithInvalidArgs(output);        
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

    public void runBaseTestsWithInvalidArgs (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("status must be false: ", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false: ", (boolean)json.get("pending"));
    }

}