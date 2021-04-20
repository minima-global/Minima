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
import org.minima.kissvm.functions.cast.HEX;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

//HEXValue HEX (BooleanValue var)
//HEXValue HEX (HEXValue var)
//HEXValue HEX (NumberValue var)
//HEXValue HEX (ScriptValue var)
public class HEXTests {

    @Test
    public void testConstructors() {
        HEX fn = new HEX();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("HEX", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("HEX");
            assertEquals("HEX", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        HEX fn = new HEX();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x01", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(false)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x00", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x01234567", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x01", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x00", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(65535)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0xFFFF", ((HexValue) res).toString()); // Test fails, because MiniNumber prepends 00
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("ABCDEFGHIJKLMNOPQRSTUVWXYZ")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x4142434445464748494A4B4C4D4E4F505152535455565758595A", ((HexValue) res).toString()); // test fails because script value forces lowercase
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0x48656C6C6F20576F726C64", ((HexValue) res).toString()); // test fails because script value forces lowercase
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        HEX fn = new HEX();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

    }
}
