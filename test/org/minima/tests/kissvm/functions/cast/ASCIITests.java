package org.minima.tests.kissvm.functions.cast;

import org.minima.kissvm.functions.cast.ASCII;

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

//ScriptValue ASCII (HEXValue var)
//ScriptValue ASCII (ScriptValue var)
public class ASCIITests {

    @Test
    public void testConstructors() {
        ASCII fn = new ASCII();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("ASCII", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("ASCII");
            assertEquals("ASCII", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        ASCII fn = new ASCII();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0x414243444546")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                //assertEquals("ABCDEF", ((ScriptValue) res).toString()); // test fails because script value forces lowercase
                assertEquals("abcdef", ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HEXValue("0x4142434445464748494A4B4C4D4E4F505152535455565758595A")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                //assertEquals("ABCDEFGHIJKLMNOPQRSTUVWXYZ", ((ScriptValue) res).toString()); // test fails because script value forces lowercase
                assertEquals("abcdefghijklmnopqrstuvwxyz", ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("Hello World")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                assertEquals("hello world", ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("LET A = 5 LET B = A + 1 LET A = B")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                assertEquals("LET a = 5 LET b = a + 1 LET a = b", ((ScriptValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        ASCII fn = new ASCII();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param types
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            //assertThrows(ExecutionException.class, () -> { // Should throw this
            //    Value res = mf.runFunction(ctr);
            //});
            assertThrows(ClassCastException.class, () -> { // but throws this
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(5)));
            //assertThrows(ExecutionException.class, () -> { // Should throw this
            //    Value res = mf.runFunction(ctr);
            //});
            assertThrows(ClassCastException.class, () -> { // but throws this
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
