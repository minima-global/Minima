package org.minima.tests.cli.burn;

import java.io.BufferedReader;
import java.io.FileReader;
import java.lang.StringBuilder;
import java.math.BigDecimal;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;

import static org.junit.Assert.*;

import org.minima.system.commands.CommandException;
import org.minima.database.MinimaDB;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import org.minima.system.Main;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;

public class DoBurnTest extends MinimaCliTest {

    @Test
    public void testBurnWithSend() throws Exception {
        
        String output = runCommand("burn");

        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse((boolean)json.get("pending"));

        var responseAttr = json.get("response");

        String newAddress = getNewAddress();
        String newAddress2 = getNewAddress();
        String newAddress3 = getNewAddress();

        boolean confirmed = waitForMinimaBlockConfirmation();

        //send to new addresses, each triggering burn event
        String sendOutput = runCommand("send address:"+newAddress+" amount:30 burn:10");
        Thread.sleep(30000);//waiting 30 seconds for transaction finality so we can see if tx is picked up with burn
        String sendOutput2 = runCommand("send address:"+newAddress2+" amount:40 burn:20"); 
        Thread.sleep(30000);//waiting 30 seconds for transaction finality so we can see if tx is picked up with burn
        String sendOutput3 = runCommand("send address:"+newAddress3+" amount:50 burn:30"); 
        Thread.sleep(30000);//waiting 30 seconds for transaction finality so we can see if tx is picked up with burn

        output = runCommand("burn");

        json = (JSONObject) new JSONParser().parse(output);
        JSONObject burnInnerResponseJSON = (JSONObject) json.get("response");
        JSONObject fiftyBlockResponse = (JSONObject) burnInnerResponseJSON.get("50block");
        assertTrue("There should be four transactions", (long)fiftyBlockResponse.get("txns") == 4); //initial minting of coins on genesis block + 3 send events is 4 txns
    }

    //burn using consolidate

    @Test
    public void testBurnWithConsolidate() throws Exception {

        boolean confirmed = waitForMinimaBlockConfirmation(); 

        String newAddress1 = getAddress();
        String addressAmount = "["+'"'+newAddress1+":10"+'"'+",";
        addressAmount += '"'+newAddress1+":20"+'"'+",";
        addressAmount += '"'+newAddress1+":30"+'"'+"]";

        String sendOutput = runCommand("send multi:"+addressAmount+" burn:50");

        Thread.sleep(90000);//waiting 1.5 minute for transaction finality so we can see if tx is picked up with burn

        String balanceResponse = runCommand("balance");

        String coinsResponse = runCommand("coins");

        String output = runCommand("consolidate tokenid:0x00");

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue("response status should be true", (boolean)json.get("status"));
    }

    //burn using tokencreate
    @Test
    public void testBurnWithTokencreate() throws Exception {
        
        boolean confirmed = waitForMinimaBlockConfirmation();

        String newAddress = getNewAddress();
        String sendOutput = runCommand("send address:"+newAddress+" amount:1000");
        Thread.sleep(60000);//waiting 1 minute for transaction finality so we can see if tx is picked up with burn
        
        String tokenCreateOutput = runCommand("tokencreate name:testtoken amount:100 burn:1");
        Thread.sleep(60000);//waiting 1 minute for transaction finality so we can see if tx is picked up with burn

        String output = runCommand("burn");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject burnInnerResponseJSON = (JSONObject) json.get("response");
        JSONObject fiftyBlockResponse = (JSONObject) burnInnerResponseJSON.get("50block");
        
        assertTrue("There should be three transactions", (long)fiftyBlockResponse.get("txns") == 3); //3 burns have happened, one when initial token supply was made, one for the send command and one for the tokencreate command
    }

    @Test
    public void testMinMaxAvgBurnStats() throws Exception {

        String newAddress = getNewAddress();
        String newAddress2 = getNewAddress();
        String newAddress3 = getNewAddress();

        boolean confirmed = waitForMinimaBlockConfirmation();

        //burn using send
        //send to random address, in this case 0xA202C7C...
        String sendOutput = runCommand("send address:"+newAddress+" amount:30 burn:10");
        Thread.sleep(30000);//waiting 1 minute for transaction finality so we can see if tx is picked up with burn
        String sendOutput2 = runCommand("send address:"+newAddress2+" amount:40 burn:20"); 
        Thread.sleep(30000);//waiting 1 minute for transaction finality so we can see if tx is picked up with burn
        String sendOutput3 = runCommand("send address:"+newAddress3+" amount:50 burn:30"); 
        Thread.sleep(30000);//waiting 1 minute for transaction finality so we can see if tx is picked up with burn

        String output = runCommand("burn");

        JSONObject json = (JSONObject) new JSONParser().parse(output);
        JSONObject burnInnerResponseJSON = (JSONObject) json.get("response");
        JSONObject fiftyBlockResponse = (JSONObject) burnInnerResponseJSON.get("50block");
        assertTrue("max should be 30",(long)fiftyBlockResponse.get("max") == 30);
        assertTrue("med should be 10",(long)fiftyBlockResponse.get("med") == 10);
        assertTrue("avg should be 15",(long)fiftyBlockResponse.get("avg") == 15);
        assertTrue("min should be 0",(long)fiftyBlockResponse.get("min") == 0);
    }

    boolean waitForMinimaBlockConfirmation() throws Exception {

        long balance = 0;
        int attempts = 0;
        while(balance == 0 && attempts <= 250){
            Thread.sleep(1000);
            String balanceOutput = runCommand("balance");

            var jsonObject =  (JSONObject) new JSONParser().parse(balanceOutput.toString());
            JSONArray innerJson = (JSONArray) jsonObject.get("response");
            JSONObject innerJsonObj = (JSONObject) innerJson.get(0);

            balance = Long.valueOf(innerJsonObj.get("confirmed").toString()).longValue();
            attempts++;
        } 
        return attempts != 250;
    }

    String runCommand(String command) throws Exception{
        super.minimaTestNode.setCommand(command);
        String output = super.minimaTestNode.runCommand();
        return output;
    }

    String getNewAddress() throws Exception{
        //get my new address
        String getaddressResponse = runCommand("newaddress");
        
        var stringResponseObj = (JSONObject) new JSONParser().parse(getaddressResponse);
        JSONObject stringResponseResponseObj = (JSONObject) stringResponseObj.get("response");
        String address = stringResponseResponseObj.get("address").toString();

        return address;
    }

    String getAddress() throws Exception{
        //get my new address
        String getaddressResponse = runCommand("getaddress");
        
        var stringResponseObj = (JSONObject) new JSONParser().parse(getaddressResponse);
        JSONObject stringResponseResponseObj = (JSONObject) stringResponseObj.get("response");
        String address = stringResponseResponseObj.get("address").toString();

        return address;
    }
}