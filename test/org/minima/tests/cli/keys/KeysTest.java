package org.minima.tests.cli.keys;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;
import static org.junit.Assert.*;

import org.minima.system.commands.CommandException;
import java.util.Arrays;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import org.minima.system.Main;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;

public class KeysTest extends MinimaCliTest {

    public MinimaTestNode test = new MinimaTestNode();

    @Test
    public void testConnectWithNoArgs () throws Exception
    {
        String output = test.runCommand("keys");

        runBaseTests(output);     
        test.killMinima();   
    }
    
    public void runBaseTests (String output) throws Exception
    {
        super.runBaseTests(output);
    }

    @Test
    public void make50Keys() throws Exception
    {
        System.out.println("Making 50 keys: ");

        Integer arr[] = { 1, 2, 5, 10, 20, 30, 49 };
        String keylist[] = {};
        boolean failed = false;

        String output = "";
        for (int i = 0; i < 50; i++) {

            if(Arrays.asList(arr).contains(i))
            {
                output = test.runCommand("keys action:list");
                var jsonObject =  (JSONObject) new JSONParser().parse(output.toString());
                JSONArray response = (JSONArray) jsonObject.get("response");
                
                if(response.size() != (keylist.length + 6)){ //we start with 6 keys when we spin up minima
                    failed = true;
                }
            }

            output = test.runCommand("keys action:new");

            JSONObject json = (JSONObject) new JSONParser().parse(output);
            JSONObject InnerResponseJSON = (JSONObject) json.get("response");
            String key = InnerResponseJSON.getString("publickey");

            keylist = push(keylist, key);
        }

        assertFalse(failed);
        test.killMinima();
    }

    private static String[] push(String[] array, String push) {
        String[] longer = new String[array.length + 1];
        System.arraycopy(array, 0, longer, 0, array.length);
        longer[array.length] = push;
        return longer;
    }
}