package org.minima.tests.objects;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

import org.minima.objects.Transaction;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class TransactionTests {

    public static final int w = 12;

    @Test
    public void testBasicTransaction() {
        // PubPrivKey k = new PubPrivKey(w*3*8);
        Transaction t = new Transaction();
        // Coin c = new Coin(Coin.MINIMA_TOKENID, 
        //                   new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
        //                   new MiniNumber("0x01"),
        //                   Coin.MINIMA_TOKENID);
        assertTrue("A new transaction should be empty", t.isEmpty());
        assertTrue("A new transaction should have no inputs", t.getAllInputs().size() == 0);
        assertTrue("A new transaction should have no outputs", t.getAllOutputs().size() == 0);
        assertTrue("A new transaction should have inputs sum = 0", t.sumInputs().isEqual(new MiniNumber(0)));
        assertTrue("A new transaction should have outputs sum = 0", t.sumOutputs().isEqual(new MiniNumber(0)));
        JSONObject json = t.toJSON();
        System.out.println("json: " + json.toJSONString());
        assertTrue("JSON has an inputs field", json.containsKey("inputs"));
        assertTrue("JSON has an outputs field", json.containsKey("outputs"));
        assertTrue("JSON has a state field", json.containsKey("state"));
        assertTrue("JSON has an linkhash field", json.containsKey("linkhash"));

        
        //t.addInput(c);
        //assertFalse("A transaction with one input should not be empty", t.isEmpty());
    }


}

