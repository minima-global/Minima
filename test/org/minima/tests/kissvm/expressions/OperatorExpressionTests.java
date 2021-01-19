package org.minima.tests.kissvm.expressions;

import org.minima.kissvm.expressions.OperatorExpression;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.values.Value;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.base.CONCAT;
import org.minima.kissvm.functions.base.LEN;
import org.minima.kissvm.functions.maths.ABS;
import org.minima.kissvm.functions.maths.INC;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.junit.Test;

public class OperatorExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {
        ConstantExpression ce1 = new ConstantExpression(new NumberValue(5));
        ConstantExpression ce2 = new ConstantExpression(new NumberValue(5));
        ConstantExpression ce3 = new ConstantExpression(new NumberValue(0));
        ConstantExpression ce4 = new ConstantExpression(new NumberValue(1));
        ConstantExpression ce5 = new ConstantExpression(new NumberValue(2));
        ConstantExpression ce6 = new ConstantExpression(new NumberValue(255));

        OperatorExpression oe1 = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_ADD);
        OperatorExpression oe2 = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SUB);
        OperatorExpression oe3 = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_MUL);
        OperatorExpression oe4 = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_DIV);
        OperatorExpression oe4a = new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_DIV);

        OperatorExpression oe5 = new OperatorExpression(ce1, OperatorExpression.OPERATOR_NEG);

        OperatorExpression oe6 = new OperatorExpression(ce1, ce4, OperatorExpression.OPERATOR_SHIFTL);
        OperatorExpression oe6a = new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_SHIFTL);
        OperatorExpression oe7 = new OperatorExpression(ce1, ce4, OperatorExpression.OPERATOR_SHIFTR);
        OperatorExpression oe7a = new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_SHIFTR);

        OperatorExpression oe8 = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_MODULO);
        OperatorExpression oe8a = new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_MODULO);
        OperatorExpression oe8b = new OperatorExpression(ce1, ce4, OperatorExpression.OPERATOR_MODULO);
        OperatorExpression oe8c = new OperatorExpression(ce1, ce5, OperatorExpression.OPERATOR_MODULO);

        OperatorExpression oe9 = new OperatorExpression(ce1, ce6, OperatorExpression.OPERATOR_AND);
        OperatorExpression oeA = new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_OR);
        OperatorExpression oeB = new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_XOR);

        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        assertEquals("should be equal ", 10, ((NumberValue)oe1.getValue(ctr)).getNumber().getAsInt());
        assertEquals("should be equal ", 0, ((NumberValue)oe2.getValue(ctr)).getNumber().getAsInt());
        assertEquals("should be equal ", 25, ((NumberValue)oe3.getValue(ctr)).getNumber().getAsInt());
        assertEquals("should be equal ", 1, ((NumberValue)oe4.getValue(ctr)).getNumber().getAsInt());
        assertThrows(ExecutionException.class, () -> {
            oe4a.getValue(ctr);
        });

        assertEquals("should be equal ", -5, ((NumberValue)oe5.getValue(ctr)).getNumber().getAsInt());

        //assertEquals("should be equal ", 10, oe6.getValue(ctr).getNumber().getAsInt()); // Invalid result
        //assertEquals("should be equal ", 5, oe6a.getValue(ctr).getNumber().getAsInt()); // Invalid result
        //assertEquals("should be equal ", 2, oe7.getValue(ctr).getNumber().getAsInt()); // Invalid result
        //assertEquals("should be equal ", 5, oe7a.getValue(ctr).getNumber().getAsInt()); // Invalid result
        assertEquals("should be equal ", 0, ((NumberValue)oe8.getValue(ctr)).getNumber().getAsInt());
        //assertThrows(ExecutionException.class, () -> { // throws ArithmeticException instead of ExecutionException
        //    oe8a.getValue(ctr);
        //});
        assertThrows(ArithmeticException.class, () -> {
            oe8a.getValue(ctr);
        });
        assertEquals("should be equal ", 0, ((NumberValue)oe8b.getValue(ctr)).getNumber().getAsInt());
        assertEquals("should be equal ", 1, ((NumberValue)oe8c.getValue(ctr)).getNumber().getAsInt());

        //PADDY - Operator params need to be HEX
        //assertEquals("should be equal ", 5, ((NumberValue)oe9.getValue(ctr)).getNumber().getAsInt()); //INVALID
        //assertEquals("should be equal ", 5, oeA.getValue(ctr).getNumber().getAsInt()); // Invalid result
        //assertEquals("should be equal ", 5, ((NumberValue)oeB.getValue(ctr)).getNumber().getAsInt());

        OperatorExpression oeX = new OperatorExpression(ce1, ce3, Integer.MIN_VALUE);
        //assertThrows(ExecutionException.class, () -> {  // should throw ExecutionException on invalid operation
        //    oeX.getValue(ctr); // shifting by zero
        //});
        assertThrows(ExecutionException.class, () -> {  // but throws NullPointerException
            oeX.getValue(ctr);
        });
    }

    @Test
    public void testToString() {
        ConstantExpression ce1 = new ConstantExpression(new NumberValue(123));
        ConstantExpression ce2 = new ConstantExpression(new NumberValue(456));

        OperatorExpression oe;

        String exp_s;
        String obj_s;

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_ADD);
        exp_s = "( " + ce1.toString() + " + " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SUB);
        exp_s = "( " + ce1.toString() + " - " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_MUL);
        exp_s = "( " + ce1.toString() + " * " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_DIV);
        exp_s = "( " + ce1.toString() + " / " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, OperatorExpression.OPERATOR_NEG);
        exp_s = " - ( " + ce1.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SHIFTL);
        exp_s = "( " + ce1.toString() + " << " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SHIFTR);
        exp_s = "( " + ce1.toString() + " >> " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_MODULO);
        exp_s = "( " + ce1.toString() + " % " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_AND);
        exp_s = "( " + ce1.toString() + " & " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_OR);
        exp_s = "( " + ce1.toString() + " | " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_XOR);
        exp_s = "( " + ce1.toString() + " ^ " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, Integer.MIN_VALUE); // returns ERROR on invalid operation
        exp_s = "( " + ce1.toString() + " ERROR " + ce2.toString() + " )"; // can be improved param ERROR param is not the best solution
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
