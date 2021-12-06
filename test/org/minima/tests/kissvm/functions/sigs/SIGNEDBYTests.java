package org.minima.tests.kissvm.functions.sigs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.sigs.SIGNEDBY;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;

//BooleanValue SIGNEDBY  (HEXValue pubkey)
public class SIGNEDBYTests {

    @Test
    public void testConstructors() {
        SIGNEDBY fn = new SIGNEDBY();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("SIGNEDBY", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("SIGNEDBY");
            assertEquals("SIGNEDBY", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        // SIGNEDBY is actually string search in a list of signatures
        // So we can simplify test without actual keys
        MiniData SingleSig1 = MiniData.getRandomData(20);
        MiniData SingleSig2 = MiniData.getRandomData(20);

        ArrayList<MiniData> signatures1 = new ArrayList<>();
        signatures1.add(SingleSig1);
        
        ArrayList<MiniData> signatures2 = new ArrayList<>();
        signatures2.add(SingleSig2);
        
        SIGNEDBY fn = new SIGNEDBY();

        {
            Contract ctr = new Contract("", signatures1, new Witness(), new Transaction(), new ArrayList<>());
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue(SingleSig1)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals("TRUE", ((BooleanValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            Contract ctr = new Contract("", signatures1, new Witness(), new Transaction(), new ArrayList<>());
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue(SingleSig2)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals("FALSE", ((BooleanValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        SIGNEDBY fn = new SIGNEDBY();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param domain
        {
        }

        // Invalid param types
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
