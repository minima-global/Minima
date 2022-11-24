package org.minima.tests.cli.send;

import java.math.BigDecimal;

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

public class SendpollTest extends MinimaCliTest {

    /*
    
    ERROR: returns status:true with no arguments
    */

    @Test
    public void testSendpollWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("sendpoll");
        runBaseTests(output);        
    }

    @Test
    public void testSendpollOneTransaction() throws Exception
    {
        testSendpollSendingToSelf(new BigDecimal("10"), 1);
    }

    @Test
    public void testSendpollTwoTransaction() throws Exception
    {
        testSendpollSendingToSelf(new BigDecimal("10"), 2);
    }

    @Test
    public void testSendpollFiveTransaction() throws Exception
    {
        testSendpollSendingToSelf(new BigDecimal("10"), 5);
    }

    public void testSendpollSendingToSelf(BigDecimal sendAmount, int numSends) throws Exception
    {
        BigDecimal initialsupply = new BigDecimal("1000000000");

        //DO SEND EVENT
        String defaultaddress = super.minimaTestNode.getAddress();
        String newaddress = super.minimaTestNode.getNewAddress();

        String sendAmountString = sendAmount.toString();

        for(int i=0; i<numSends; i++){
            String output = super.minimaTestNode.runCommand("sendpoll address:"+newaddress+" amount:"+sendAmountString);
        }

        int currentBlock = super.minimaTestNode.getCurrentBlock();
        int blockToConfirmation = currentBlock + (6*numSends);

        //WAIT FOR CONFIRMATIONS
        while(currentBlock < blockToConfirmation)
        {
            currentBlock = super.minimaTestNode.getCurrentBlock();
            Thread.sleep(1000);
        }

        String balanceOutput = super.minimaTestNode.runCommand("balance");
        //CHECK BALANCE OF RECEIVER
        JSONObject json = (JSONObject) new JSONParser().parse(balanceOutput);
        var responseAttr = json.get("response");

        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);        
        BigDecimal confirmed = new BigDecimal(responseInnerJson.get("confirmed").toString()); 

        assertTrue("Sending to yourself: final amount is incorrect: "+confirmed.toString()+" vs "+initialsupply.toString(), confirmed.compareTo(initialsupply) == 0);
    }

    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be false: " + json.get("status"));
        assertFalse((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean)json.get("pending"));
    }

}

