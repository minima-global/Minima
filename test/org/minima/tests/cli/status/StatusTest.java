package org.minima.tests.cli.status;

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

public class StatusTest extends MinimaCliTest {

    public MinimaTestNode test = new MinimaTestNode();

    @Test
    public void testStatusWithNoArgs () throws Exception
    {
        String output = test.runCommand("status");

        runBaseTests(output);        
    }
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}