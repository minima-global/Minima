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

import org.minima.system.Main;

public class DoBalanceTest {

    public BalanceTest test = new BalanceTest();

    @Test
    public void testBalanceWithNoArgs() throws Exception {
        
        test.setCommand("balance");

        String output = test.runCommand();

        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertTrue((boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse((boolean)json.get("pending"));

        var responseAttr = json.get("response");

        //The response body must be valid JSON
        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject responseInnerJson = (JSONObject) jsonArray.get(0);

        //make sure sendable, unconfirmed and confirmed are all >= 0
        BigDecimal ZERO = new BigDecimal("0");
        BigDecimal confirmed = new BigDecimal(responseInnerJson.get("confirmed").toString()); 
        BigDecimal unconfirmed = new BigDecimal(responseInnerJson.get("unconfirmed").toString()); 
        BigDecimal sendable = new BigDecimal(responseInnerJson.get("sendable").toString()); 
        BigDecimal total = new BigDecimal(responseInnerJson.get("total").toString());

        //Confirmed coins cannot be negative
        assertTrue(confirmed.compareTo(ZERO) >= 0);

        //Unconfirmed coins cannot be negative
        assertTrue(unconfirmed.compareTo(ZERO) >= 0);

        //Spendable coins cannot be negative
        assertTrue(sendable.compareTo(ZERO) >= 0);

        //Total coins cannot be negative
        assertTrue(total.compareTo(ZERO) >= 0);

        //confirmed, unconfirmed and spendable must equal total
        assertTrue(confirmed.add(unconfirmed).add(confirmed).add(sendable).equals(total));
       
    }

    @After public void killMinima() throws Exception {
         test.minima.runMinimaCMD("quit",false);
    }

}