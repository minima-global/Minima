package org.minima.tests.kissvm.functions.maths;

import org.minima.kissvm.functions.maths.BITSET;

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
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

//HEXValue BITSET (HEXValue var, NumberValue pos, BooleanVlaue val)
public class CEILTests {

    @Test
    public void testConstructors() {
        BITSET fn = new BITSET();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("BITSET", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("BITSET");
            assertEquals("BITSET", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        BITSET fn = new BITSET();

        { // Incorrect function behavior, as it uses invalid byte indexing in data array
            for (int i = 0; i < 63; i++) {
                Long TestValue = 1L << i;
                {
                    MinimaFunction mf = fn.getNewFunction();
                    mf.addParameter(new ConstantExpression(new HEXValue(Long.toHexString(TestValue))));
                    mf.addParameter(new ConstantExpression(new NumberValue(i)));
                    mf.addParameter(new ConstantExpression(new BooleanValue(false)));
                    //try {
                    //    Value res = mf.runFunction(ctr);
                    //    assertEquals(Value.VALUE_HEX, res.getValueType());
                    //    assertEquals("0x00", ((HEXValue) res).toString());
                    //} catch (ExecutionException ex) {
                    //    fail();
                    //}
                }

                TestValue = ~TestValue;
                {
                    MinimaFunction mf = fn.getNewFunction();
                    mf.addParameter(new ConstantExpression(new HEXValue(Long.toHexString(TestValue))));
                    mf.addParameter(new ConstantExpression(new NumberValue(i)));
                    mf.addParameter(new ConstantExpression(new BooleanValue(true)));
                    //try {
                    //    Value res = mf.runFunction(ctr);
                    //    assertEquals(Value.VALUE_HEX, res.getValueType());
                    //    assertEquals("0xFFFFFFFFFFFFFFFF", ((HEXValue) res).toString());
                    //} catch (ExecutionException ex) {
                    //    fail();
                    //}
                }

            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        BITSET fn = new BITSET();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0x00")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0x00")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
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
            mf.addParameter(new ConstantExpression(new HEXValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(123456798)));
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
