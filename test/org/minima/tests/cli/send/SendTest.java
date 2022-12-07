package org.minima.tests.cli.send;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SendTest extends MinimaCliTest {

    @Test
    public void testSendWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("send");
        runBaseTests(output);
    }

    @Test
    public void testSending10() throws Exception {
        testSendingToSelf(new BigDecimal("10"));
    }

    @Test
    public void testSending50000() throws Exception {
        testSendingToSelf(new BigDecimal("50000"));
    }

    @Test
    public void testSendingDecimalCase1() throws Exception {
        testSendingToSelf(new BigDecimal("1.999999999999999999"));
    }

    @Test
    public void testSendingDecimalCase2() throws Exception {
        testSendingToSelf(new BigDecimal("1.000000000000000001"));
    }

    @Test
    public void testSendingZero() throws Exception {
        testSendingToSelf(new BigDecimal("0"));
    }

    @Test
    public void testSendingNegative() throws Exception {
        testSendingToSelf(new BigDecimal("-10"));
    }

    @Test
    public void testSendingWithExponentNotation() throws Exception {
        testSendingToSelf(new BigDecimal("1e-18"));
    }

    @Test
    public void testSendingAllFunds() throws Exception {
        testSendingToSelf(new BigDecimal("1000000000"));
    }

    @Test
    public void testSendingTooMuch() throws Exception {
        testSendingToSelf(new BigDecimal("1000000001"));
    }

    @Test
    public void testSendingMassiveNumber() throws Exception {
        testSendingToSelf(new BigDecimal("100000000000000000000000000000000000000000000000000000000000000000000000"));
    }

    public void testSendingToSelf(BigDecimal sendAmount) throws Exception {
        BigDecimal initialsupply = new BigDecimal("1000000000");

        //DO SEND EVENT
        String defaultaddress = minimaTestNode.getAddress();
        String newaddress = minimaTestNode.getNewAddress();

        String sendAmountString = sendAmount.toString();
        String output = minimaTestNode.runCommand("send address:" + newaddress + " amount:" + sendAmountString);

        int currentBlock = minimaTestNode.getCurrentBlock();
        int blockToConfirmation = currentBlock + 6;

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
        assertFalse((boolean) json.get("status"));

        //cmd response pending should be false
        assertFalse((boolean) json.get("pending"));
    }

}