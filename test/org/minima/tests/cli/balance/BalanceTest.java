package org.minima.tests.cli.balance;
import java.io.BufferedReader;
import java.io.FileReader;
import java.lang.StringBuilder;
import java.math.BigDecimal;
import org.junit.Test;
import org.junit.After;
import static org.junit.Assert.*;
import org.minima.system.commands.CommandException;
import org.minima.database.MinimaDB;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import java.nio.charset.Charset;
import java.util.Random; 

import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.system.Main;

public class BalanceTest extends MinimaCliTest {

    public MinimaTestNode minimaTestNode = new MinimaTestNode();
    
    public BigDecimal ZERO = new BigDecimal("0");

    public void runBaseTests (String output) throws Exception
    {
        super.runBaseTests(output);

        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);
        var responseAttr = json.get("response");

        //The response body must be valid JSON
        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        //make sure sendable, unconfirmed and confirmed are all >= 0
        
        BigDecimal confirmed = new BigDecimal(responseInnerJson.get("confirmed").toString()); 
        BigDecimal unconfirmed = new BigDecimal(responseInnerJson.get("unconfirmed").toString()); 
        BigDecimal sendable = new BigDecimal(responseInnerJson.get("sendable").toString()); 
        BigDecimal total = new BigDecimal(responseInnerJson.get("total").toString());

        //Confirmed coins cannot be negative
        System.out.println("confirmed must be gte 0");
        assertTrue(confirmed.compareTo(ZERO) >= 0);

        //Unconfirmed coins cannot be negative
        System.out.println("unconfirmed must be gte 0");
        assertTrue(unconfirmed.compareTo(ZERO) >= 0);

        //Spendable coins cannot be negative
        System.out.println("sendable must be gte 0");
        assertTrue(sendable.compareTo(ZERO) >= 0);

        //Total coins cannot be negative
        System.out.println("total must be gte 0");
        assertTrue(total.compareTo(ZERO) >= 0);

        //confirmed, unconfirmed and spendable must not be gt total
        System.out.println("confirmed must be lte total");
        assertTrue(confirmed.compareTo(total) < 1);

        System.out.println("sendable must be lte total");
        assertTrue(sendable.compareTo(total) < 1);

        System.out.println("unconfirmed must be lte total");
        assertTrue(unconfirmed.compareTo(total) < 1);
    }

    @Test
    public void testBalanceWithNoArgs() throws Exception {
        
        minimaTestNode.setCommand("balance");

        String output = minimaTestNode.runCommand();

        //tests that apply to every balance command
        runBaseTests (output);

        killMinima();
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

        System.out.println("Regular 0x address\n");
        System.out.println(output);

        runBaseTests (output);        
        
        //Run tests with Mx address
        minimaTestNode.setCommand("balance address:" + myMiniAddress);

        output = minimaTestNode.runCommand();

        System.out.println("Mx address\n");
        System.out.println(output);

        runBaseTests (output);     

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

        System.out.println("The output is ");
        System.out.println(output.toString());

        runBaseTests (output);
        
        System.out.println("Testing with garbage ZALGO text. He comes. \n");

        String zalgo = " ̵̨̨͕̥̼̤̪͉̬̟̪͙̮͙̳͈͉̹̗̳̳͍͔̤͐̆̓͊̃̒̿̈́̽͌̀̎̃̓̎͛̌̓̒̾͑͆͘͘͜͝ ̶̢̨̢̪̯̩̙̗̫͕̘̦͉̮̗̹̼̳̭̱͈̭̮̠͙̻̫̆̎̎̈́̈̒̂͗̐̏͆̓̿̐͒͂͌̍͜͝ͅͅ";

        minimaTestNode.setCommand("balance address:" + zalgo);

        output = minimaTestNode.runCommand();

        runBaseTests (output);

        //Test with extra garbage arguments

        minimaTestNode.setCommand("balance address:0x422033899FD23B3B8EFECCBDE05D1D40D1550E8BB27798C12820C0D2CFC83689 garbage:garble");

        output = minimaTestNode.runCommand();

        json = (JSONObject) new JSONParser().parse(output);
        
        //status of the cmd request must be true
        System.out.println("status must be false");
        assertFalse((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:");
        assertFalse((boolean)json.get("pending"));
        
    }

    @Test
    public void testBalanceWithGarbageArg() throws Exception {

        System.out.println("Testing with garbage argument");

        minimaTestNode.setCommand("balance garbage:mygarbage");

        String output = minimaTestNode.runCommand();

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be false");
        assertFalse((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:");
        assertFalse((boolean)json.get("pending"));

    }

    @Test
    public void testBalanceWithEvilQuotes() throws Exception {

        minimaTestNode.setCommand("balance \"");
        String output = minimaTestNode.runCommand();
        JSONObject json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        minimaTestNode.setCommand("balance \"\"");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));
        
        System.out.println("Testing balance with evil quotes");

        /*
        ----------------------------
        !!! THIS TEST IS FAILING !!!
        ----------------------------
        */

        minimaTestNode.setCommand("balance \"\"\"");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        minimaTestNode.setCommand("balance \'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        minimaTestNode.setCommand("balance \'\'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        minimaTestNode.setCommand("balance \'\'\'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        minimaTestNode.setCommand("balance \'\'\'\'");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

    }

    @Test
    public void testBalanceWithMultipleArg() throws Exception {

        System.out.println("Testing with garbage argument");

        minimaTestNode.setCommand("balance address:0x7F234423354BF8E172F1019C7ED64310FB2EE7C9F5AFDA640F28D49F8D64DCDC confirmations:1");

        String output = minimaTestNode.runCommand();

        runBaseTests (output);

    }

    @Test
    public void testBalanceWithDuplicateAddressArg() throws Exception {

        System.out.println("Testing with garbage argument");

        minimaTestNode.setCommand("balance address:0x7F234423354BF8E172F1019C7ED64310FB2EE7C9F5AFDA640F28D49F8D64DCDC address:0x7F234423354BF8E172F1019C7ED64310FB2EE7C9F5AFDA640F28D49F8D64DCDC");

        String output = minimaTestNode.runCommand();

        runBaseTests (output);
        
    }

    @Test
    public void testBalanceWithConfirmationsArg() throws Exception {

        System.out.println("Testing balance with confirmation argument \n");
        minimaTestNode.setCommand("balance confirmations:0");
        String output = minimaTestNode.runCommand();
        runBaseTests (output);


        System.out.println("Test with 1000000000000000000000000000000000000000000000000000000000000000000000 confirmations");
        minimaTestNode.setCommand("balance confirmations:1000000000000000000000000000000000000000000000000000000000000000000000");
        output = minimaTestNode.runCommand();
        JSONObject json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        System.out.println("Test with floating point confirmations");
        minimaTestNode.setCommand("balance confirmations:0.01");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        /*
        ----------------------------
        !!! THIS TEST IS FAILING !!!
        ----------------------------
        */
        System.out.println("Test with non numeric confirmations");
        minimaTestNode.setCommand("balance confirmations:a");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

        System.out.println("Test with extra garbage args");
        minimaTestNode.setCommand("balance confirmations:1 garble:garble");
        output = minimaTestNode.runCommand();
        json = (JSONObject) new JSONParser().parse(output);
        assertFalse((boolean)json.get("status"));
        assertFalse((boolean)json.get("pending"));

    }

    public void killMinima(){
         minimaTestNode.minima.runMinimaCMD("quit",false);
    }

}