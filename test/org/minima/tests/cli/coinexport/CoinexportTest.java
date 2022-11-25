package org.minima.tests.cli.coinexport;

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

public class CoinexportTest extends MinimaCliTest {
    
    public void baseTests (String output) throws Exception
    {
        super.runBaseTests(output);
    }

    @Test
    public void coinImportWithCoinid() throws Exception
    {
        boolean confirmed = super.minimaTestNode.waitForMinimaBlockConfirmation(); 

        //run coins
        String coinsOutput = super.minimaTestNode.runCommand("coins");

        JSONObject json = (JSONObject) new JSONParser().parse(coinsOutput);
        var responseAttr = json.get("response");

        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        var coinid = responseInnerJson.get("coinid");

        //coinexport with coinid
        String output = super.minimaTestNode.runCommand("coinexport coinid:"+coinid.toString());

        baseTests(output);
    }

}