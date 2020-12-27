package org.minima.tests.kissvm.expressions;

import org.minima.kissvm.expressions.VariableExpression;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.junit.Test;

public class VariableExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {
        VariableExpression ve1 = new VariableExpression("BooleanValue");
        VariableExpression ve2 = new VariableExpression("HEXValue");
        VariableExpression ve3 = new VariableExpression("NumberValue");
        VariableExpression ve4 = new VariableExpression("ScriptValue");

        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<StateVariable>());

        assertThrows(ExecutionException.class, () -> {
            ve1.getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> {
            ve2.getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> {
            ve3.getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> {
            ve4.getValue(ctr);
        });

        BooleanValue bv = new BooleanValue(true);
        HEXValue hv = new HEXValue(new MiniData());
        NumberValue nv = new NumberValue(0);
        ScriptValue sv = new ScriptValue("");

        ctr.setVariable("BooleanValue", bv);
        ctr.setVariable("HEXValue", hv);
        ctr.setVariable("NumberValue", nv);
        ctr.setVariable("ScriptValue", sv);

        assertEquals("should be equal ", bv, ve1.getValue(ctr));
        assertEquals("should be equal ", hv, ve2.getValue(ctr));
        assertEquals("should be equal ", nv, ve3.getValue(ctr));
        assertEquals("should be equal ", sv, ve4.getValue(ctr));
    }

    @Test
    public void testToString() throws ExecutionException {
        VariableExpression ge1 = new VariableExpression("BooleanValue");
        VariableExpression ge2 = new VariableExpression("HEXValue");
        VariableExpression ge3 = new VariableExpression("NumberValue");
        VariableExpression ge4 = new VariableExpression("ScriptValue");

        String exp_s;
        String obj_s;

        exp_s = "variable:BooleanValue";
        obj_s = ge1.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        exp_s = "variable:HEXValue";
        obj_s = ge2.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        exp_s = "variable:NumberValue";
        obj_s = ge3.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        exp_s = "variable:ScriptValue";
        obj_s = ge4.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
