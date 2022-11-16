package org.minima.tests.kissvm.statements.commands;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.statements.commands.EXECstatement;
import org.minima.kissvm.values.StringValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

public class EXECstatementTests {

    @Test
    public void testConstructors() {
        ConstantExpression Empty = new ConstantExpression(new StringValue(""));
        ConstantExpression ReturnTrue = new ConstantExpression(new StringValue("RETURN TRUE"));
        ConstantExpression ReturnFalse = new ConstantExpression(new StringValue("RETURN FALSE"));

        EXECstatement es1 = new EXECstatement(Empty);
        assertEquals("EXEC ", es1.toString());

        EXECstatement es2 = new EXECstatement(ReturnTrue);
        assertEquals("EXEC RETURN TRUE", es2.toString());

        EXECstatement es3 = new EXECstatement(ReturnFalse);
        assertEquals("EXEC RETURN FALSE", es3.toString());
    }

    @Test
    public void testExecution() {
        ConstantExpression Empty = new ConstantExpression(new StringValue(""));
        ConstantExpression ReturnTrue = new ConstantExpression(new StringValue("RETURN TRUE"));
        ConstantExpression ReturnFalse = new ConstantExpression(new StringValue("RETURN FALSE"));
        ConstantExpression Garbage = new ConstantExpression(new StringValue("Hello World"));

        {
            EXECstatement es = new EXECstatement(Empty);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            //assertThrows(ExecutionException.class, () -> { // should throw this
            //    ifs.execute(ctr);
            //});

            // but does not throw
            try {
                es.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
            assertEquals(0, ctr.getNumberOfInstructions());
        }
        {
            EXECstatement es = new EXECstatement(ReturnTrue);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                es.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            EXECstatement es = new EXECstatement(ReturnFalse);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                es.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            EXECstatement es = new EXECstatement(Garbage);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            assertThrows(ExecutionException.class, () -> {
                es.execute(ctr);
            });
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
            assertEquals(0, ctr.getNumberOfInstructions());
        }
    }
}
