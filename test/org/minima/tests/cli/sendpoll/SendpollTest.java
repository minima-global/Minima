package org.minima.tests.cli.sendpoll;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SendpollTest extends MinimaCliTest {

    /*
    
    ERROR: returns status:true with no arguments
    */

    @Test
    public void testSendpollWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("sendpoll");
        runBaseTests(output);
    }

    @Test
    public void testSendpollOneTransaction() throws Exception {
        testSendpollSendingToSelf(new BigDecimal("10"), 1);
    }

    @Test
    public void testSendpollTwoTransaction() throws Exception {
        testSendpollSendingToSelf(new BigDecimal("10"), 2);
    }

    @Test
    public void testSendpollFiveTransaction() throws Exception {
        testSendpollSendingToSelf(new BigDecimal("10"), 5);
    }

    public void testSendpollSendingToSelf(BigDecimal sendAmount, int numSends) throws Exception {
        BigDecimal initialsupply = new BigDecimal("1000000000");

        //DO SEND EVENT
        String defaultaddress = minimaTestNode.getAddress();
        String newaddress = minimaTestNode.getNewAddress();

        String sendAmountString = sendAmount.toString();

        for (int i = 0; i < numSends; i++) {
            String output = minimaTestNode.runCommand("sendpoll address:" + newaddress + " amount:" + sendAmountString);
        }

        int currentBlock = minimaTestNode.getCurrentBlock();
        int blockToConfirmation = currentBlock + (6 * numSends);

        //WAIT FOR CONFIRMATIONS
        while (currentBlock < blockToConfirmation) {
            currentBlock = minimaTestNode.getCurrentBlock();
            Thread.sleep(1000);
        }

        String balanceOutput = minimaTestNode.runCommand("balance");
        //CHECK BALANCE OF RECEIVER
        JSONObject json = (JSONObject) new JSONParser().parse(balanceOutput);
        var responseAttr = json.get("response");

        var jsonArray = (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);
        BigDecimal confirmed = new BigDecimal(responseInnerJson.get("confirmed").toString());

        assertTrue(confirmed.compareTo(initialsupply) == 0, "Sending to yourself: final amount is incorrect: " + confirmed.toString() + " vs " + initialsupply.toString());
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be false: " + json.get("status"));
        assertFalse((boolean) json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean) json.get("pending"));
    }

}

