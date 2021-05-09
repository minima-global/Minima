package org.minima.tests.kissvm.functions.sha;

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
import org.minima.kissvm.functions.sha.SHA2;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.utils.Crypto;

//HEXValue SHA2 (HEXValue data)
//HEXValue SHA2 (ScriptValue data)
public class SHA2Tests {

    @Test
    public void testConstructors() {
        SHA2 fn = new SHA2();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("SHA2", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("SHA2");
            assertEquals("SHA2", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        SHA2 fn = new SHA2();

        {
            for (int i = 0; i < 100; i++) {
                HexValue Param = new HexValue(MiniData.getRandomData(64).to0xString());
                HexValue Result = new HexValue(Crypto.getInstance().hashSHA2(Param.getRawData()));

                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(Param));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_HEX, res.getValueType());
                    assertEquals(Result.toString(), ((HexValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
        }

        {
            HexValue Param = new HexValue("");
            HexValue Result = new HexValue(Crypto.getInstance().hashSHA2(Param.getRawData()));

            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(Param));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals(Result.toString(), ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("")));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals("0xE3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855", ((HexValue) res).toString());
            } catch (ExecutionException ex) {
                fail();
            }
        }

    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        SHA2 fn = new SHA2();

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
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

    }
}
