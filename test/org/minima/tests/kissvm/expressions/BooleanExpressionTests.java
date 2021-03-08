package org.minima.tests.kissvm.expressions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.BooleanExpression;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

public class BooleanExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {

        ConstantExpression cet = new ConstantExpression(new BooleanValue(true));
        ConstantExpression cef = new ConstantExpression(new BooleanValue(false));

        BooleanExpression be;

        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        be = new BooleanExpression(cet, cet, BooleanExpression.BOOLEAN_AND);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cet, cef, BooleanExpression.BOOLEAN_AND);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cef, cet, BooleanExpression.BOOLEAN_AND);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cef, cef, BooleanExpression.BOOLEAN_AND);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(cet, cet, BooleanExpression.BOOLEAN_NAND);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cet, cef, BooleanExpression.BOOLEAN_NAND);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cef, cet, BooleanExpression.BOOLEAN_NAND);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cef, cef, BooleanExpression.BOOLEAN_NAND);
        assertTrue(be.getValue(ctr).isTrue());

        be = new BooleanExpression(cet, cet, BooleanExpression.BOOLEAN_OR);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cet, cef, BooleanExpression.BOOLEAN_OR);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cef, cet, BooleanExpression.BOOLEAN_OR);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cef, cef, BooleanExpression.BOOLEAN_OR);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(cet, cet, BooleanExpression.BOOLEAN_NOR);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cet, cef, BooleanExpression.BOOLEAN_NOR);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cef, cet, BooleanExpression.BOOLEAN_NOR);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cef, cef, BooleanExpression.BOOLEAN_NOR);
        assertTrue(be.getValue(ctr).isTrue());

        be = new BooleanExpression(cet, cet, BooleanExpression.BOOLEAN_XOR);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cet, cef, BooleanExpression.BOOLEAN_XOR);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cef, cet, BooleanExpression.BOOLEAN_XOR);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cef, cef, BooleanExpression.BOOLEAN_XOR);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(cet, cet, BooleanExpression.BOOLEAN_NXOR);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(cet, cef, BooleanExpression.BOOLEAN_NXOR);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cef, cet, BooleanExpression.BOOLEAN_NXOR);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(cef, cef, BooleanExpression.BOOLEAN_NXOR);
        assertTrue(be.getValue(ctr).isTrue());

        be = new BooleanExpression(cet, BooleanExpression.BOOLEAN_NOT);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(cef, BooleanExpression.BOOLEAN_NOT);
        assertTrue(be.getValue(ctr).isTrue());

        ConstantExpression ce1 = new ConstantExpression(new NumberValue(1));
        ConstantExpression ce2 = new ConstantExpression(new NumberValue(2));

        be = new BooleanExpression(ce1, ce1, BooleanExpression.BOOLEAN_EQ);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_EQ);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(ce2, ce1, BooleanExpression.BOOLEAN_EQ);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(ce1, ce1, BooleanExpression.BOOLEAN_NEQ);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_NEQ);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(ce2, ce1, BooleanExpression.BOOLEAN_NEQ);
        assertTrue(be.getValue(ctr).isTrue());

        be = new BooleanExpression(ce1, ce1, BooleanExpression.BOOLEAN_LT);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_LT);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(ce2, ce1, BooleanExpression.BOOLEAN_LT);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(ce1, ce1, BooleanExpression.BOOLEAN_LTE);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_LTE);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(ce2, ce1, BooleanExpression.BOOLEAN_LTE);
        assertTrue(be.getValue(ctr).isFalse());

        be = new BooleanExpression(ce1, ce1, BooleanExpression.BOOLEAN_GT);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_GT);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(ce2, ce1, BooleanExpression.BOOLEAN_GT);
        assertTrue(be.getValue(ctr).isTrue());

        be = new BooleanExpression(ce1, ce1, BooleanExpression.BOOLEAN_GTE);
        assertTrue(be.getValue(ctr).isTrue());
        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_GTE);
        assertTrue(be.getValue(ctr).isFalse());
        be = new BooleanExpression(ce2, ce1, BooleanExpression.BOOLEAN_GTE);
        assertTrue(be.getValue(ctr).isTrue());

        BooleanExpression be1 = new BooleanExpression(ce1, ce2, Integer.MIN_VALUE);
        //assertThrows(ExecutionException.class, () -> {  // should throw ExecutionException on invalid operation
        //    be1.getValue(ctr);
        //});
        assertThrows(ExecutionException.class, () -> {  // but throws NullPointerException
            be1.getValue(ctr);
        });
    }

    @Test
    public void testToString() {
        ConstantExpression ce1 = new ConstantExpression(new BooleanValue(true));
        ConstantExpression ce2 = new ConstantExpression(new BooleanValue(false));

        BooleanExpression be;

        String exp_s;
        String obj_s;

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_AND);
        exp_s = "( " + ce1.toString() + " AND " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_NAND);
        exp_s = "( " + ce1.toString() + " NAND " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_OR);
        exp_s = "( " + ce1.toString() + " OR " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_NOR);
        exp_s = "( " + ce1.toString() + " NOR " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_XOR);
        exp_s = "( " + ce1.toString() + " XOR " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_NXOR);
        exp_s = "( " + ce1.toString() + " NXOR " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_EQ);
        exp_s = "( " + ce1.toString() + " EQ " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_NEQ);
        exp_s = "( " + ce1.toString() + " NEQ " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_LT);
        exp_s = "( " + ce1.toString() + " LT " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_LTE);
        exp_s = "( " + ce1.toString() + " LTE " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_GT);
        exp_s = "( " + ce1.toString() + " GT " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_GTE);
        exp_s = "( " + ce1.toString() + " GTE " + ce2.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, BooleanExpression.BOOLEAN_NOT);
        exp_s = "NOT ( " + ce1.toString() + " )";
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        be = new BooleanExpression(ce1, ce2, Integer.MIN_VALUE); // returns ERROR on invalid operation
        exp_s = "( " + ce1.toString() + " ERROR " + ce2.toString() + " )"; // can be improved param ERROR param is not the best solution
        obj_s = be.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
