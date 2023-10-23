package org.minima.tests.cli.coins;

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

public class CoinsTest extends MinimaCliTest {

    public MinimaTestNode test = new MinimaTestNode();

    @Test
    public void runBaseTests () throws Exception
    {
        boolean confirmed = test.waitForMinimaBlockConfirmation(); 

        //run coins
        String coinsOutput = test.runCommand("coins");
        super.runBaseTests(coinsOutput);
    }

}