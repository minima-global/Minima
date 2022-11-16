package org.minima.tests.cli.balance;
import java.io.BufferedReader;
import java.io.FileReader;
import java.lang.StringBuilder;

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

    public BalanceTest t = new BalanceTest();

    

    @Test
    public void testBalanceWithNoArgs() throws Exception {
        
        String output = t.runCommand();

        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status must be true
        assertTrue((boolean)json.get("status"));

        //response pending should be false
        assertFalse((boolean)json.get("pending"));

        var responseAttr = json.get("response");

        //This will throw an exception if the response is not an array
        var jsonArray =  (JSONArray) new JSONParser().parse(responseAttr.toString());
        JSONObject json = (JSONObject) jsonArray.get(0);
       
    }

    @After public void killMinima() throws Exception {
         t.minima.runMinimaCMD("quit",false);
    }

}