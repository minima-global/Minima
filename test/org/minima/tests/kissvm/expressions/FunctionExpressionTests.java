package org.minima.tests.kissvm.expressions;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.FunctionExpression;
import org.minima.kissvm.functions.hex.CONCAT;
import org.minima.kissvm.functions.hex.LEN;
import org.minima.kissvm.functions.number.ABS;
import org.minima.kissvm.functions.number.INC;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

public class FunctionExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {

        HexValue hv1 = new HexValue("0x01234567");
        HexValue hv2 = new HexValue("0x89");
        NumberValue nv1 = new NumberValue(10);
        NumberValue nv2 = new NumberValue(-10);
        StringValue sv1 = new StringValue("HELLO");
        StringValue sv2 = new StringValue("WORLD");

        CONCAT fCONCAT = new CONCAT();
        fCONCAT.addParameter(new ConstantExpression(hv1));
        fCONCAT.addParameter(new ConstantExpression(hv2));

        LEN fLEN = new LEN();
        fLEN.addParameter(new ConstantExpression(hv1));

        ABS fABS = new ABS();
        fABS.addParameter(new ConstantExpression(nv2));

        INC fINC = new INC();
        fINC.addParameter(new ConstantExpression(nv1));

        FunctionExpression fe1 = new FunctionExpression(fCONCAT);
        FunctionExpression fe2 = new FunctionExpression(fLEN);
        FunctionExpression fe3 = new FunctionExpression(fABS);
        FunctionExpression fe4 = new FunctionExpression(fINC);

        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        assertEquals("should be equal ", "0x0123456789", fe1.getValue(ctr).toString());
        assertEquals("should be equal ", "HELLO WORLD", Value.getValue("[HELLO WORLD]").toString());
        assertEquals("should be equal ", 4, ((NumberValue) fe2.getValue(ctr)).getNumber().getAsInt());
        assertEquals("should be equal ", 10, ((NumberValue) fe3.getValue(ctr)).getNumber().getAsInt());
        assertEquals("should be equal ", 11, ((NumberValue) fe4.getValue(ctr)).getNumber().getAsInt());
    }

    @Test
    public void testToString() {

        NumberValue nv1 = new NumberValue(10);
        NumberValue nv2 = new NumberValue(-10);
        StringValue sv1 = new StringValue("[HELLO]");
        StringValue sv2 = new StringValue("[WORLD]");

        CONCAT fCONCAT = new CONCAT();
        fCONCAT.addParameter(new ConstantExpression(sv1));
        fCONCAT.addParameter(new ConstantExpression(sv2));

        LEN fLEN = new LEN();
        fLEN.addParameter(new ConstantExpression(sv1));

        ABS fABS = new ABS();
        fABS.addParameter(new ConstantExpression(nv2));

        INC fINC = new INC();
        fINC.addParameter(new ConstantExpression(nv1));

        FunctionExpression fe1 = new FunctionExpression(fCONCAT);
        FunctionExpression fe2 = new FunctionExpression(fLEN);
        FunctionExpression fe3 = new FunctionExpression(fABS);
        FunctionExpression fe4 = new FunctionExpression(fINC);

        String exp_s;
        String obj_s;

        exp_s = "function:" + fCONCAT.getName() + ", params:" + fCONCAT.getAllParameters();
        obj_s = fe1.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        exp_s = "function:" + fLEN.getName() + ", params:" + fLEN.getAllParameters();
        obj_s = fe2.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        exp_s = "function:" + fABS.getName() + ", params:" + fABS.getAllParameters();
        obj_s = fe3.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        exp_s = "function:" + fINC.getName() + ", params:" + fINC.getAllParameters();
        obj_s = fe4.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
