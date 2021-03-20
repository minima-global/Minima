package org.minima.tests.kissvm.statements.commands;

import org.minima.kissvm.statements.commands.EXECstatement;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.BooleanExpression;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.OperatorExpression;
import org.minima.kissvm.expressions.VariableExpression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.commands.LETstatement;
import org.minima.kissvm.statements.commands.RETURNstatement;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

public class EXECstatementTests {

    @Test
    public void testConstructors() {
        ConstantExpression Empty = new ConstantExpression(new ScriptValue(""));
        ConstantExpression ReturnTrue = new ConstantExpression(new ScriptValue("RETURN TRUE"));
        ConstantExpression ReturnFalse = new ConstantExpression(new ScriptValue("RETURN FALSE"));

        EXECstatement es1 = new EXECstatement(Empty);
        assertEquals("EXEC ", es1.toString());

        EXECstatement es2 = new EXECstatement(ReturnTrue);
        assertEquals("EXEC RETURN TRUE", es2.toString());

        EXECstatement es3 = new EXECstatement(ReturnFalse);
        assertEquals("EXEC RETURN FALSE", es3.toString());
    }

    @Test
    public void testExecution() {
        ConstantExpression Empty = new ConstantExpression(new ScriptValue(""));
        ConstantExpression ReturnTrue = new ConstantExpression(new ScriptValue("RETURN TRUE"));
        ConstantExpression ReturnFalse = new ConstantExpression(new ScriptValue("RETURN FALSE"));
        ConstantExpression Garbage = new ConstantExpression(new ScriptValue("Hello World"));

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
            assertThrows(ExecutionException.class, () -> { // should throw this
                es.execute(ctr);
            });
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
            assertEquals(0, ctr.getNumberOfInstructions());
        }
    }
}
