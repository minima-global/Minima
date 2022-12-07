package org.minima.objects;

import org.junit.jupiter.api.Test;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TransactionTests {

    public static final int w = 12;

    @Test
    public void testBasicTransaction() {
        // PubPrivKey k = new PubPrivKey(w*3*8);
        Transaction t = new Transaction();
        assertTrue(t.isEmpty(), "A new transaction should be empty");
        assertTrue(t.getAllInputs().size() == 0, "A new transaction should have no inputs");
        assertTrue(t.getAllOutputs().size() == 0, "A new transaction should have no outputs");
        assertTrue(t.sumInputs().isEqual(new MiniNumber(0)), "A new transaction should have inputs sum = 0");
        assertTrue(t.sumOutputs().isEqual(new MiniNumber(0)), "A new transaction should have outputs sum = 0");
        JSONObject json = t.toJSON();
        assertTrue(json.containsKey("inputs"), "JSON has an inputs field");
        assertTrue(json.containsKey("outputs"), "JSON has an outputs field");
        assertTrue(json.containsKey("state"), "JSON has a state field");
        assertTrue(json.containsKey("linkhash"), "JSON has an linkhash field");
    }

    @Test
    public void testMinimaTransaction() {
        // PubPrivKey k = new PubPrivKey(w*3*8);
        Transaction t = new Transaction();
        Coin c = new Coin(Coin.COINID_OUTPUT,
                new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
                new MiniNumber(5),
                Token.TOKENID_MINIMA);
        assertTrue(t.isEmpty(), "A new transaction should be empty");
        t.addInput(c);

        assertFalse(t.isEmpty(), "A transaction with one input should not be empty");
        assertTrue(t.sumInputs().isEqual(new MiniNumber(5)), "The transaction input sum should be 5");

        Coin c1 = new Coin(Coin.COINID_OUTPUT,
                new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
                new MiniNumber(2),
                Token.TOKENID_MINIMA);
        t.addOutput(c1);

        assertTrue(t.getAllOutputs().size() == 1, "The transaction should have one output");
        assertTrue(t.sumOutputs().isEqual(new MiniNumber(2)), "The transaction output sum should be 2");
        assertTrue(t.checkValid(), "The transaction is valid (more inputs than outputs)");
        Coin c2 = new Coin(Coin.COINID_OUTPUT,
                new MiniData("0x2B1B3FD8AD198A5C9B0A8B376C73766B73ACE408E0A2537B827EC8DCFF4133BC"),
                new MiniNumber(3),
                Token.TOKENID_MINIMA);
        t.addOutput(c2);
        assertTrue(t.getAllOutputs().size() == 2, "The transaction should have two outputs");
        assertTrue(t.sumOutputs().isEqual(new MiniNumber(5)), "The transaction output sum should be 5");
        assertTrue(t.checkValid(), "The transaction is valid");
        for (int i = 0; i < 256; i++) {
            // maybe stateExists should limit input values to 0-255
            assertFalse(t.stateExists(i), "Transaction should have no state existing");
        }
        String HelloWorld = "[HelloWorld]";
        t.addStateVariable(new StateVariable(1, HelloWorld));
        assertTrue(t.stateExists(1), "Transaction should have state port 1 defined");
        assertTrue(t.getStateValue(1).toString().compareTo(HelloWorld) == 0, "Variable set should match original String");

        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            t.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            Transaction tread = new Transaction();
            tread.readDataStream(dis);

            assertTrue(tread.getAllOutputs().size() == 2, "The transaction should have two outputs");
            assertTrue(tread.sumOutputs().isEqual(new MiniNumber(5)), "The transaction output sum should be 5");
            assertTrue(tread.checkValid(), "The transaction is valid");

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(false, " there should not be an IOException");
        }

    }
}
