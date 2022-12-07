package org.minima.tests.kissvm.expressions;


import org.junit.jupiter.api.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniNumber;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ConstantExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {
        BooleanValue bv = new BooleanValue(true);
        HexValue hv = new HexValue(new MiniNumber(Integer.valueOf(255)));
        NumberValue nv = new NumberValue(0x12345678);
        StringValue sv = new StringValue("[RETURN TRUE]");

        ConstantExpression ce1 = new ConstantExpression(bv);
        ConstantExpression ce2 = new ConstantExpression(hv);
        ConstantExpression ce3 = new ConstantExpression(nv);
        ConstantExpression ce4 = new ConstantExpression(sv);

        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<StateVariable>());

        assertEquals(bv, ce1.getValue(ctr), "should be equal ");
        assertEquals(hv, ce2.getValue(ctr), "should be equal ");
        assertEquals(nv, ce3.getValue(ctr), "should be equal ");
        assertEquals(sv, ce4.getValue(ctr), "should be equal ");
    }

    @Test
    public void testToString() throws ExecutionException {
        BooleanValue bv = new BooleanValue(true);
        HexValue hv = new HexValue(new MiniNumber(Integer.valueOf(255)));
        NumberValue nv = new NumberValue(0x12345678);
        StringValue sv = new StringValue("[RETURN TRUE]");

        ConstantExpression ce1 = new ConstantExpression(bv);
        ConstantExpression ce2 = new ConstantExpression(hv);
        ConstantExpression ce3 = new ConstantExpression(nv);
        ConstantExpression ce4 = new ConstantExpression(sv);

        String exp_s;
        String obj_s;

        exp_s = bv.toString();
        obj_s = ce1.toString();
        assertEquals(exp_s, obj_s, "should be equal ");

        exp_s = hv.toString();
        obj_s = ce2.toString();
        assertEquals(exp_s, obj_s, "should be equal ");

        exp_s = nv.toString();
        obj_s = ce3.toString();
        assertEquals(exp_s, obj_s, "should be equal ");

        exp_s = sv.toString();
        obj_s = ce4.toString();
        assertEquals(exp_s, obj_s, "should be equal ");
    }

}
