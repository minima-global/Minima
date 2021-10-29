package org.minima.tests.kissvm.functions.state;

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
import org.minima.kissvm.functions.state.STATE;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

//BooleanValue STATE (NumberValue statenum)
//HEXValue STATE (NumberValue statenum)
//NumberValue STATE (NumberValue statenum)
//ScriptValue STATE (NumberValue statenum)
public class STATETests {

    @Test
    public void testConstructors() {
        STATE fn = new STATE();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("STATE", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("STATE");
            assertEquals("STATE", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
        ArrayList<StateVariable> States = new ArrayList<StateVariable>();
        for (int i = 0; i < 16; i++) {
            States.add(new StateVariable(4 * i + 0, new BooleanValue(true).toString()));
            States.add(new StateVariable(4 * i + 1, new HexValue("0x12345678").toString()));
            States.add(new StateVariable(4 * i + 2, new NumberValue(i).toString()));
            States.add(new StateVariable(4 * i + 3, new StringValue("[ Hello World " + Integer.toString(4 * i + 3)).toString() + " ]"));
        }

        Transaction Trx = new Transaction();
        for (StateVariable sv : States) {
            Trx.addStateVariable(sv);
        }

        Contract ctr = new Contract("", "", new Witness(), Trx, new ArrayList<>());

        STATE fn = new STATE();

        for (int i = 0; i < States.size(); i++) {
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                try {
                    Value res = mf.runFunction(ctr);
                    if (i % 4 == 0) {
                        assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                        assertEquals(Value.getValue(States.get(i).toString()).toString(), ((BooleanValue) res).toString());
                    }
                    if (i % 4 == 1) {
                        assertEquals(Value.VALUE_HEX, res.getValueType());
                        assertEquals(Value.getValue(States.get(i).toString()).toString(), ((HexValue) res).toString());
                    }
                    if (i % 4 == 2) {
                        assertEquals(Value.VALUE_NUMBER, res.getValueType());
                        assertEquals(Value.getValue(States.get(i).toString()).toString(), ((NumberValue) res).toString());
                    }
                    if (i % 4 == 3) {
                        assertEquals(Value.VALUE_SCRIPT, res.getValueType());
                        assertEquals(Value.getValue(States.get(i).toString()).toString(), ((StringValue) res).toString());
                    }

                } catch (ExecutionException ex) {
                    fail();
                }
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        STATE fn = new STATE();

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

        // Invalid param domain
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
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
