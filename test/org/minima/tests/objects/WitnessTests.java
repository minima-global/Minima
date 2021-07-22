package org.minima.tests.objects;

import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.objects.Token;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.proofs.ScriptProof;
import org.minima.utils.json.JSONObject;

public class WitnessTests {

    @Test
    public void testWitness() {
        Witness witness = new Witness();
        witness.addTempTokenDetails(new Token(new MiniData("0x00ff"),
                new MiniNumber(4),
                new MiniNumber(1000),
                new MiniString("TestToken"),
                new MiniString("")));
//        assertTrue("There should be 1 token detail", witness.getAllTokenDetails().size() == 1);
        witness.getTokenDetail(new MiniData("0x00ff"));
        try {
            witness.addScript(new ScriptProof("test-string", 160));
            JSONObject json = witness.toJSON();
            System.out.println("json: " + json.toJSONString());
            assertTrue("JSON object not empty", json != null);
        } catch (Exception e) {
            assertTrue("Exception should not happen", false);
        }
    }

}
