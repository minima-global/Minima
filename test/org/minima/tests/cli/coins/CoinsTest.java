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

    @Test
    public void runBaseTests () throws Exception
    {
        boolean confirmed = super.minimaTestNode.waitForMinimaBlockConfirmation(); 

        //run coins
        String coinsOutput = super.minimaTestNode.runCommand("coins");
        super.runBaseTests(coinsOutput);
    }

}