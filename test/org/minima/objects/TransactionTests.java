package org.minima.objects;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class TransactionTests {

    public static final int w = 12;

    @Test
    public void testBasicTransaction() {
        // PubPrivKey k = new PubPrivKey(w*3*8);
        Transaction t = new Transaction();
        assertTrue("A new transaction should be empty", t.isEmpty());
        assertTrue("A new transaction should have no inputs", t.getAllInputs().size() == 0);
        assertTrue("A new transaction should have no outputs", t.getAllOutputs().size() == 0);
        assertTrue("A new transaction should have inputs sum = 0", t.sumInputs().isEqual(new MiniNumber(0)));
        assertTrue("A new transaction should have outputs sum = 0", t.sumOutputs().isEqual(new MiniNumber(0)));
        JSONObject json = t.toJSON();
        assertTrue("JSON has an inputs field", json.containsKey("inputs"));
        assertTrue("JSON has an outputs field", json.containsKey("outputs"));
        assertTrue("JSON has a state field", json.containsKey("state"));
        assertTrue("JSON has an linkhash field", json.containsKey("linkhash"));
    }

    @Test
    public void testMinimaTransaction() {
        // PubPrivKey k = new PubPrivKey(w*3*8);
        Transaction t = new Transaction();
        Coin c = new Coin(Coin.COINID_OUTPUT,
                new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
                new MiniNumber(5),
                Token.TOKENID_MINIMA);
        assertTrue("A new transaction should be empty", t.isEmpty());
        t.addInput(c);

        assertFalse("A transaction with one input should not be empty", t.isEmpty());
        assertTrue("The transaction input sum should be 5", t.sumInputs().isEqual(new MiniNumber(5)));

        Coin c1 = new Coin(Coin.COINID_OUTPUT,
                new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
                new MiniNumber(2),
                Token.TOKENID_MINIMA);
        t.addOutput(c1);

        assertTrue("The transaction should have one output", t.getAllOutputs().size() == 1);
        assertTrue("The transaction output sum should be 2", t.sumOutputs().isEqual(new MiniNumber(2)));
        assertTrue("The transaction is valid (more inputs than outputs)", t.checkValid());
        Coin c2 = new Coin(Coin.COINID_OUTPUT,
                new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
                new MiniNumber(3),
                Token.TOKENID_MINIMA);
        t.addOutput(c2);
        assertTrue("The transaction should have two outputs", t.getAllOutputs().size() == 2);
        assertTrue("The transaction output sum should be 5", t.sumOutputs().isEqual(new MiniNumber(5)));
        assertTrue("The transaction is valid", t.checkValid());
        for (int i = 0; i < 256; i++) {
            // maybe stateExists should limit input values to 0-255
            assertFalse("Transaction should have no state existing", t.stateExists(i));
        }
        String HelloWorld = "[HelloWorld]";
        t.addStateVariable(new StateVariable(1, HelloWorld));
        assertTrue("Transaction should have state port 1 defined", t.stateExists(1));
        assertTrue("Variable set should match original String", t.getStateValue(1).toString().compareTo(HelloWorld) == 0);

        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            t.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            Transaction tread = new Transaction();
            tread.readDataStream(dis);

            assertTrue("The transaction should have two outputs", tread.getAllOutputs().size() == 2);
            assertTrue("The transaction output sum should be 5", tread.sumOutputs().isEqual(new MiniNumber(5)));
            assertTrue("The transaction is valid", tread.checkValid());
            
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }
}
