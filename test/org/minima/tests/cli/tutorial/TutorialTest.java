package org.minima.tests.cli.tutorial;

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

public class TutorialTest extends MinimaCliTest {

    @Test
    public void testTutorialWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("tutorial");
        runBaseTests(output);        
    }
    
    public void runBaseTests (String output) throws Exception
    {
        super.runBaseTests(output);
    }

}