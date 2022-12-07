package org.minima.tests.cli.coinexport;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class CoinexportTest extends MinimaCliTest {

    public void baseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

    @Test
    public void coinImportWithCoinid() throws Exception {
        boolean confirmed = minimaTestNode.waitForMinimaBlockConfirmation();

        //run coins
        String coinsOutput = minimaTestNode.runCommand("coins");

        JSONObject json = (JSONObject) new JSONParser().parse(coinsOutput);
        var responseAttr = json.get("response");

        var jsonArray = (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        var coinid = responseInnerJson.get("coinid");

        //coinexport with coinid
        String output = minimaTestNode.runCommand("coinexport coinid:" + coinid.toString());

        baseTests(output);
    }

}