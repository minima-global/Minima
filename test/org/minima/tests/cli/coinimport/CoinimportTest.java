package org.minima.tests.cli.coinimport;


import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import static org.junit.jupiter.api.Assertions.assertFalse;

public class CoinimportTest extends MinimaCliTest {

    @Test
    public void coinImportWithData() throws Exception {
        boolean confirmed = minimaTestNode.waitForMinimaBlockConfirmation();

        //run coins
        String coinsOutput = minimaTestNode.runCommand("coins");

        System.out.println("coinsOutput: ");
        System.out.println(coinsOutput);

        JSONObject json = (JSONObject) new JSONParser().parse(coinsOutput);
        var responseAttr = json.get("response");

        var jsonArray = (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        var coinid = responseInnerJson.get("coinid");

        minimaTestNode.runCommand("cointrack enable:false coinid:" + coinid);

        //coinexport with coinid
        String exported = minimaTestNode.runCommand("coinexport coinid:" + coinid.toString());

        JSONObject exportjson = (JSONObject) new JSONParser().parse(exported);

        System.out.println("exported: ");
        System.out.println(exportjson.get("response"));

        String output = minimaTestNode.runCommand("coinimport data:" + exportjson.get("response"));

        System.out.println("output: ");
        System.out.println(output);

        json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status")); //MMR proof already tracked

        runBaseTests(output);

    }

}