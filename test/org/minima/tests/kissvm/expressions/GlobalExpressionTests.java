package org.minima.tests.kissvm.expressions;


import org.junit.jupiter.api.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.GlobalExpression;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class GlobalExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {
        GlobalExpression ge1 = new GlobalExpression("BooleanValue");
        GlobalExpression ge2 = new GlobalExpression("HEXValue");
        GlobalExpression ge3 = new GlobalExpression("NumberValue");
        GlobalExpression ge4 = new GlobalExpression("ScriptValue");

        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<StateVariable>());

        assertThrows(ExecutionException.class, () -> {
            ge1.getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> {
            ge2.getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> {
            ge3.getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> {
            ge4.getValue(ctr);
        });

        BooleanValue bv = new BooleanValue(true);
        HexValue hv = new HexValue(new MiniData());
        NumberValue nv = new NumberValue(0);
        StringValue sv = new StringValue("[]");

        ctr.setGlobalVariable("BooleanValue", bv);
        ctr.setGlobalVariable("HEXValue", hv);
        ctr.setGlobalVariable("NumberValue", nv);
        ctr.setGlobalVariable("ScriptValue", sv);

        assertEquals(bv, ge1.getValue(ctr), "should be equal ");
        assertEquals(hv, ge2.getValue(ctr), "should be equal ");
        assertEquals(nv, ge3.getValue(ctr), "should be equal ");
        assertEquals(sv, ge4.getValue(ctr), "should be equal ");
    }

    @Test
    public void testToString() throws ExecutionException {
        GlobalExpression ge1 = new GlobalExpression("BooleanValue");
        GlobalExpression ge2 = new GlobalExpression("HEXValue");
        GlobalExpression ge3 = new GlobalExpression("NumberValue");
        GlobalExpression ge4 = new GlobalExpression("ScriptValue");

        String exp_s;
        String obj_s;

        exp_s = "global:BooleanValue";
        obj_s = ge1.toString();
        assertEquals(exp_s, obj_s, "should be equal ");

        exp_s = "global:HEXValue";
        obj_s = ge2.toString();
        assertEquals(exp_s, obj_s, "should be equal ");

        exp_s = "global:NumberValue";
        obj_s = ge3.toString();
        assertEquals(exp_s, obj_s, "should be equal ");

        exp_s = "global:ScriptValue";
        obj_s = ge4.toString();
        assertEquals(exp_s, obj_s, "should be equal ");
    }

}
