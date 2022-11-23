package org.minima.tests.cli.coinimport;

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

public class CoinimportTest extends MinimaCliTest {

    public void baseTests (String output) throws Exception
    {
        super.runBaseTests(output);
    }

    @Test
    public void coinImportWithData () throws Exception
    {
        boolean confirmed = super.minimaTestNode.waitForMinimaBlockConfirmation(); 

        //run coins
        String coinsOutput = super.minimaTestNode.runCommand("coins");

        System.out.println("coinsOutput: ");
        System.out.println(coinsOutput);

        JSONObject json = (JSONObject) new JSONParser().parse(coinsOutput);
        var responseAttr = json.get("response");

        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        var coinid = responseInnerJson.get("coinid");

        super.minimaTestNode.runCommand("cointrack enable:false coinid:"+coinid);

        //coinexport with coinid
        String exported = super.minimaTestNode.runCommand("coinexport coinid:"+coinid.toString());

        JSONObject exportjson = (JSONObject) new JSONParser().parse(exported);

        System.out.println("exported: ");
        System.out.println(exportjson.get("response"));

        String output = super.minimaTestNode.runCommand("coinimport data:"+exportjson.get("response"));

        System.out.println("output: ");
        System.out.println(output);

        json = (JSONObject) new JSONParser().parse(output);
        
        assertFalse((boolean)json.get("status")); //MMR proof already tracked
        
        baseTests(output);

    }

}