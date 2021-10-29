package org.minima.tests.kissvm.functions.maths;

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
import org.minima.kissvm.functions.hex.BITCOUNT;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

//NumberValue BITCOUNT (HEXValue var)
public class BITCOUNTTests {

    @Test
    public void testConstructors() {
        BITCOUNT fn = new BITCOUNT();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("BITCOUNT", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("BITCOUNT");
            assertEquals("BITCOUNT", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        BITCOUNT fn = new BITCOUNT();

        int[] BitsPerByte = new int[256];
        for (int i = 0; i < 256; i++) {
            int BitCount = 0;
            int CurrentValue = i;
            while (CurrentValue > 0) {
                if (CurrentValue % 2 == 1) {
                    BitCount = BitCount + 1;
                }
                CurrentValue = CurrentValue / 2;
            }
            BitsPerByte[i] = BitCount;
        }
        for (int i = 0; i < 256; i++) {
            if (BitsPerByte[i] != BITCOUNT.BITSPERBYTE[i]) {
                fail();
            }
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x00")));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x01")));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x02")));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x04")));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x08")));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x0F")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("4", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0xF0")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("4", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0xFF")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("8", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0xFF0")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("8", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0xFF8")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("9", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0xFFF")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("12", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("128", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x1248")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_NUMBER, res.getValueType());
                assertEquals("4", ((NumberValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        BITCOUNT fn = new BITCOUNT();

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
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
