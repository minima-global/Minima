package org.minima.tests.kissvm.functions.tokens;

import org.minima.kissvm.functions.tokens.TOKENSCRIPT;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.MultiKey;
import org.minima.objects.proofs.TokenProof;

import java.util.ArrayList;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

//ScriptValue TOKENSCRIPT (HEXValue tokenid)
public class TOKENSCRIPTTests {

    @Test
    public void testConstructors() {
        TOKENSCRIPT fn = new TOKENSCRIPT();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("TOKENSCRIPT", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("TOKENSCRIPT");
            assertEquals("TOKENSCRIPT", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {

        TokenProof tp1 = new TokenProof(MiniData.getRandomData(16),
                MiniNumber.EIGHT,
                MiniNumber.THOUSAND,
                new MiniString("TestToken1"),
                new MiniString("Hello from TestToken1"));

        TokenProof tp2 = new TokenProof(MiniData.getRandomData(16),
                MiniNumber.TEN,
                MiniNumber.MILLION,
                new MiniString("TestToken2"),
                new MiniString("Hello from TestToken2"));

        TokenProof tp3 = new TokenProof(MiniData.getRandomData(16),
                MiniNumber.TWELVE,
                MiniNumber.BILLION,
                new MiniString("TestToken3"),
                new MiniString("Hello from TestToken3"));

        Witness w1 = new Witness();
        w1.addTokenDetails(tp1);
        w1.addTokenDetails(tp2);
        w1.addTokenDetails(tp3);

        Contract ctr = new Contract("", "", w1, new Transaction(), new ArrayList<>());

        TOKENSCRIPT fn = new TOKENSCRIPT();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue(tp1.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                assertEquals(tp1.getTokenScript().toString().toLowerCase(), ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue(tp2.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                assertEquals(tp2.getTokenScript().toString().toLowerCase(), ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue(tp3.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                assertEquals(tp3.getTokenScript().toString().toLowerCase(), ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        TOKENSCRIPT fn = new TOKENSCRIPT();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0x12345678")));
            mf.addParameter(new ConstantExpression(new HEXValue("0x12345678")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param domain
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
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
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
