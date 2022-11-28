package org.minima.tests.cli.getaddress;

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

public class GetaddressTest extends MinimaCliTest {

    @Test
    public void testGetaddressWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("getaddress");
        runBaseTests(output);  
    }
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}