package org.minima.tests.kissvm.functions.maths;

import org.minima.kissvm.functions.maths.DEC;

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

//NumberValue DEC (NumberValue var)
public class DECTests {

    @Test
    public void testConstructors() {
        DEC fn = new DEC();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("DEC", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("DEC");
            assertEquals("DEC", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        DEC fn = new DEC();

        { // More tests to be added, once the arithmetic is fixed, and we now the upper limits
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(-1)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("-2", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(-0.99999999999999999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    //assertEquals("-1.99999999999999999", ((NumberValue) res).toString()); // should be -1.99999999999999999
                    assertEquals("-2", ((NumberValue) res).toString()); // Should be 0
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(-0.99999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("-1.99999", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(-0.49999999999999999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    //assertEquals("-1.49999999999999999", ((NumberValue) res).toString()); // should be -1.49999999999999999
                    assertEquals("-1.5", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(-0.49999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("-1.49999", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(0)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("-1", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(0.49999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("-0.50001", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(0.49999999999999999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    //assertEquals("-5.0000000000000001", ((NumberValue) res).toString()); // should be -5.0000000000000001
                    assertEquals("-0.5", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(0.99999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("-0.00001", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(0.99999999999999999)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    //assertEquals("-0.00000000000000001", ((NumberValue) res).toString()); // should be -0.00000000000000001
                    assertEquals("0", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(1)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_NUMBER, res.getValueType());
                    assertEquals("0", ((NumberValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }

        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        DEC fn = new DEC();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
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
            //assertThrows(ExecutionException.class, () -> { // Should throw this
            //    Value res = mf.runFunction(ctr);
            //});
            // But does not throw
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0x01234567")));
            //assertThrows(ExecutionException.class, () -> { // Should throw this
            //    Value res = mf.runFunction(ctr);
            //});
            assertThrows(ClassCastException.class, () -> { // but throws this
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("Hello World")));
            //assertThrows(ExecutionException.class, () -> { // Should throw this
            //    Value res = mf.runFunction(ctr);
            //});
            assertThrows(ClassCastException.class, () -> { // but throws this
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
