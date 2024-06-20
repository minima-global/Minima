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

    public MinimaTestNode test = new MinimaTestNode();

    public void baseTests (String output) throws Exception
    {
        super.runBaseTests(output);
    }

    @Test
    public void coinImportWithData () throws Exception
    {
        boolean confirmed = test.waitForMinimaBlockConfirmation(); 

        //run coins
        String coinsOutput = test.runCommand("coins");

        System.out.println("coinsOutput: ");
        System.out.println(coinsOutput);

        JSONObject json = (JSONObject) new JSONParser().parse(coinsOutput);
        var responseAttr = json.get("response");

        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        var coinid = responseInnerJson.get("coinid");

        test.runCommand("cointrack enable:false coinid:"+coinid);

        //coinexport with coinid
        String exported = test.runCommand("coinexport coinid:"+coinid.toString());

        JSONObject exportjson = (JSONObject) new JSONParser().parse(exported);

        System.out.println("exported: ");
        System.out.println(exportjson.get("response"));

        String output = test.runCommand("coinimport data:"+exportjson.get("response"));

//{"command":"coinimport","params":{"data":{"command":"coinexport","params":{"coinid":"0x4A53698FC9721731817BAB4488ED58F5B6CBEF110AD951F6F53374EB9F30C5F9"},"status":true,"pending":false,"response":"0x000000204A53698FC9721731817BAB4488ED58F5B6CBEF110AD951F6F53374EB9F30C5F9000000205EC75319945DED777D1B7B8A4603484D5E2D2D1C92F9BF6CA8D34FC950C32C4FF70101000000010001000100000000010100000101000100000001010001010100000020A53AD6A229F11DAD05FD0D9164F8EEE96501FA67ED9FEF7087EEA5C43E293357000100"}},"status":false,"pending":false,"error":"class org.minima.utils.json.JSONObject cannot be cast to class java.lang.String (org.minima.utils.json.JSONObject is in unnamed module of loader 'app'; java.lang.String is in module java.base of loader 'bootstrap')"}

        System.out.println("output: ");
        System.out.println(output);

        json = (JSONObject) new JSONParser().parse(output);
        
        assertFalse((boolean)json.get("status")); //MMR proof already tracked
        //baseTests(output);
    }

}