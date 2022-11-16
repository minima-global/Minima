package org.minima.tests.kissvm.functions.state;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.state.SAMESTATE;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

//BooleanValue SAMESTATE (NumberValue start NumberValue end)
public class SAMESTATETests {

    @Test
    public void testConstructors() {
        SAMESTATE fn = new SAMESTATE();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("SAMESTATE", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("SAMESTATE");
            assertEquals("SAMESTATE", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {

        ArrayList<StateVariable> PrevStates = new ArrayList<StateVariable>();
        for (int i = 0; i < 16; i++) {
            PrevStates.add(new StateVariable(4 * i + 0, new BooleanValue(true).toString()));
            PrevStates.add(new StateVariable(4 * i + 1, new HexValue("0x12345678").toString()));
            PrevStates.add(new StateVariable(4 * i + 2, new NumberValue(i).toString()));
            PrevStates.add(new StateVariable(4 * i + 3, new StringValue("[ Hello World " + Integer.toString(4 * i + 3)).toString() + " ]"));
        }

        ArrayList<StateVariable> SameStates = new ArrayList<StateVariable>();
        for (int i = 0; i < 16; i++) {
            SameStates.add(new StateVariable(4 * i + 0, new BooleanValue(true).toString()));
            SameStates.add(new StateVariable(4 * i + 1, new HexValue("0x12345678").toString()));
            SameStates.add(new StateVariable(4 * i + 2, new NumberValue(i).toString()));
            SameStates.add(new StateVariable(4 * i + 3, new StringValue("[ Hello World " + Integer.toString(4 * i + 3)).toString() + " ]"));
        }

        ArrayList<StateVariable> DfrnStates = new ArrayList<StateVariable>();
        for (int i = 0; i < 16; i++) {
            DfrnStates.add(new StateVariable(4 * i + 0, new StringValue("[ Hello World " + Integer.toString(4 * i + 0)).toString() + " ]"));
            DfrnStates.add(new StateVariable(4 * i + 1, new NumberValue(i).toString()));
            DfrnStates.add(new StateVariable(4 * i + 2, new HexValue("0x12345678").toString()));
            DfrnStates.add(new StateVariable(4 * i + 3, new BooleanValue(true).toString()));
        }

        SAMESTATE fn = new SAMESTATE();

        Transaction Trx1 = new Transaction();
        for (StateVariable sv : SameStates) {
            Trx1.addStateVariable(sv);
        }

        Contract ctr1 = new Contract("", "", new Witness(), Trx1, PrevStates);

        for (int i = 0; i < PrevStates.size(); i++) {
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                try {
                    Value res = mf.runFunction(ctr1);
                    assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                    assertTrue(((BooleanValue) res).isTrue());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
        }

        Transaction Trx2 = new Transaction();
        for (StateVariable sv : DfrnStates) {
            Trx2.addStateVariable(sv);
        }

        Contract ctr2 = new Contract("", "", new Witness(), Trx2, PrevStates);

        for (int i = 0; i < DfrnStates.size(); i++) {
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                try {
                    Value res = mf.runFunction(ctr2);
                    assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                    assertTrue(((BooleanValue) res).isFalse());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        SAMESTATE fn = new SAMESTATE();

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
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
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
            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
            mf.addParameter(new ConstantExpression(new NumberValue(-5)));
            assertThrows(ExecutionException.class, () -> { // Should throw this
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(100)));
            mf.addParameter(new ConstantExpression(new NumberValue(200)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param types
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
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
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

    }
}
