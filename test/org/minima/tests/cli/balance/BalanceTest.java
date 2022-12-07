package org.minima.tests.cli.balance;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class BalanceTest extends MinimaCliTest {

    public MinimaTestNode minimaTestNode = new MinimaTestNode();

    public BigDecimal ZERO = new BigDecimal("0");

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);

        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);
        var responseAttr = json.get("response");

        //The response body must be valid JSON
        var jsonArray = (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        //make sure sendable, unconfirmed and confirmed are all >= 0

        BigDecimal confirmed = new BigDecimal(responseInnerJson.get("confirmed").toString());
        BigDecimal unconfirmed = new BigDecimal(responseInnerJson.get("unconfirmed").toString());
        BigDecimal sendable = new BigDecimal(responseInnerJson.get("sendable").toString());
        BigDecimal total = new BigDecimal(responseInnerJson.get("total").toString());

        assertTrue(confirmed.compareTo(ZERO) >= 0, "confirmed must be gte 0");

        assertTrue(unconfirmed.compareTo(ZERO) >= 0, "unconfirmed must be gte 0");

        assertTrue(sendable.compareTo(ZERO) >= 0, "sendable must be gte 0");

        assertTrue(total.compareTo(ZERO) >= 0, "total must be gte 0");

        assertTrue(unconfirmed.compareTo(total) < 1, "unconfirmed must be lte total");

        assertTrue(sendable.compareTo(total) < 1, "sendable must be lte total");

        assertTrue(unconfirmed.compareTo(total) < 1, "unconfirmed must be lte total");
    }

    @Test
    public void testBalanceWithNoArgs() throws Exception {

        minimaTestNode.setCommand("balance");

        String output = minimaTestNode.runCommand();

        //tests that apply to every balance command
        runBaseTests(output);

    }

    @Test
    public void testBalanceWithAddressArg() throws Exception {

        //First get the 0x address and the miniaddress
        minimaTestNode.setCommand("getaddress");

        String myAddress = "";
        String myMiniAddress = "";

        String output = minimaTestNode.runCommand();

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        var responseAttr = json.get("response");

        JSONObject responseObj = (JSONObject) new JSONParser().parse(responseAttr.toString());

        myAddress = responseObj.get("address").toString();
        myMiniAddress = responseObj.get("miniaddress").toString();

        //Run tests with 0x address
        minimaTestNode.setCommand("balance address: " + myAddress);

        output = minimaTestNode.runCommand();

        runBaseTests(output);

        //Run tests with Mx address
        minimaTestNode.setCommand("balance address:" + myMiniAddress);

        output = minimaTestNode.runCommand();

        runBaseTests(output);

        /* 
        ----------------------------
        !!! THIS TEST IS FAILING !!!
        ----------------------------
        */

        //Try an invalid address of 1,000,000 characters
        byte[] array = new byte[1000000];
        new Random().nextBytes(array);
        String aMillionRandomCharacters = new String(array, Charset.forName("UTF-8"));

        minimaTestNode.setCommand("balance address:" + aMillionRandomCharacters);

        output = minimaTestNode.runCommand();

        runBaseTests(output);

        //Test with extra garbage arguments

        minimaTestNode.setCommand("balance address:0x422033899FD23B3B8EFECCBDE05D1D40D1550E8BB27798C12820C0D2CFC83689 garbage:garble");

        output = minimaTestNode.runCommand();

        json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "status must be false");

        assertFalse((boolean) json.get("pending"), "pending must be false");

    }

    @Test
    public void testBalanceWithGarbageArg() throws Exception {

        minimaTestNode.setCommand("balance garbage:mygarbage");

        String output = minimaTestNode.runCommand();

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "status must be false");

        assertFalse((boolean) json.get("pending"), "pending must be false");
    }

    @Test
    public void testBalanceWithEvilQuotes() throws Exception {

        minimaTestNode.setCommand("balance \"");
        String output = minimaTestNode.runCommand();
        JSONObject json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");

        minimaTestNode.setCommand("balance \"\"");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");
        
        /*
        ----------------------------
        !!! THIS TEST IS FAILING !!!
        ----------------------------
        */

        minimaTestNode.setCommand("balance \"\"\"");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");

        minimaTestNode.setCommand("balance \'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");
        minimaTestNode.setCommand("balance \'\'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");

        minimaTestNode.setCommand("balance \'\'\'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");

        minimaTestNode.setCommand("balance \'\'\'\'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "status must be false");
        assertFalse((boolean) json.get("pending"), "pending must be false");

    }

    @Test
    public void testBalanceWithMultipleArg() throws Exception {

        minimaTestNode.setCommand("balance address:0x7F234423354BF8E172F1019C7ED64310FB2EE7C9F5AFDA640F28D49F8D64DCDC confirmations:1");

        String output = minimaTestNode.runCommand();

        runBaseTests(output);
    }

    @Test
    public void testBalanceWithDuplicateAddressArg() throws Exception {

        minimaTestNode.setCommand("balance address:0x7F234423354BF8E172F1019C7ED64310FB2EE7C9F5AFDA640F28D49F8D64DCDC address:0x7F234423354BF8E172F1019C7ED64310FB2EE7C9F5AFDA640F28D49F8D64DCDC");

        String output = minimaTestNode.runCommand();

        runBaseTests(output);
    }

    @Test
    public void testBalanceWithConfirmationsArg() throws Exception {

        minimaTestNode.setCommand("balance confirmations:0");
        String output = minimaTestNode.runCommand();
        runBaseTests(output);


        minimaTestNode.setCommand("balance confirmations:1000000000000000000000000000000000000000000000000000000000000000000000");
        output = minimaTestNode.runCommand();
        JSONObject json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "Status should be false");
        assertFalse((boolean) json.get("pending"), "Pending should be false");

        minimaTestNode.setCommand("balance confirmations:0.01");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean) json.get("status"), "Status should be false");
        assertFalse((boolean) json.get("pending"), "Pending should be false");

        /*
        ----------------------------
        !!! THIS TEST IS FAILING !!!
        ----------------------------
        */
        minimaTestNode.setCommand("balance confirmations:a");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "Status should be false");
        assertFalse((boolean) json.get("pending"), "Pending should be false");

        minimaTestNode.setCommand("balance confirmations:1 garble:garble");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);

        assertFalse((boolean) json.get("status"), "Status should be false");
        assertFalse((boolean) json.get("pending"), "Pending should be false");

    }

}