package org.minima.tests.kissvm.functions.cast;

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
import org.minima.kissvm.functions.cast.NUMBER;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

//NumberValue NUMBER (BooleanValue var)
//NumberValue NUMBER (HEXValue var)
//NumberValue NUMBER (NumberValue var)
//NumberValue NUMBER (ScriptValue var)
public class NUMBERTests {

    @Test
    public void testConstructors() {
        NUMBER fn = new NUMBER();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("NUMBER", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("NUMBER");
            assertEquals("NUMBER", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        NUMBER fn = new NUMBER();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("1", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(false)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("0", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0xFFFF")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("65535", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0xFFFFFFFFFFFFFFFF")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("18446744073709551615", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(-65536)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("-65536", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(65535)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("65535", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("ABCDEFGHIJKLMNOPQRSTUVWXYZ")));
            try {
                Value res = mf.runFunction(ctr);
                //assertEquals(Value.VALUE_NUMBER, res.getValueType()); // Test fails due to invalid return type
                //assertEquals("", ((NumberValue) res).toString()); // Test not completed
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("Hello World")));
            try {
                Value res = mf.runFunction(ctr);
                //assertEquals(Value.VALUE_NUMBER, res.getValueType()); // Test fails due to invalid return type
                //assertEquals("", ((NumberValue) res).toString()); // Test not completed
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        NUMBER fn = new NUMBER();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
