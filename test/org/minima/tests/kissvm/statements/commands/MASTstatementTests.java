package org.minima.tests.kissvm.statements.commands;

import org.minima.kissvm.statements.commands.MASTstatement;

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
import org.minima.objects.base.MiniData;
import org.minima.objects.proofs.ScriptProof;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

public class MASTstatementTests {

    @Test
    public void testConstructors() {
        ConstantExpression Empty = new ConstantExpression(new HEXValue(""));
        ConstantExpression Random = new ConstantExpression(new HEXValue("0xBC05B9280323A07AE6B2C6FA183F20991D38D01BC91F02C439CB5A33113E106C"));

        MASTstatement ms1 = new MASTstatement(Empty);
        assertEquals("MAST ", ms1.toString());

        MASTstatement ms2 = new MASTstatement(Random);
        assertEquals("MAST 0xBC05B9280323A07AE6B2C6FA183F20991D38D01BC91F02C439CB5A33113E106C", ms2.toString());
    }

    @Test
    public void testExecution() {
        {
            try {
                ScriptProof sp = new ScriptProof("", 256);
                MASTstatement ms = new MASTstatement(new ConstantExpression(new HEXValue(sp.getFinalHash())));

                Witness w = new Witness();
                w.addScript(sp);
                Contract ctr = new Contract("", "", w, new Transaction(), new ArrayList<>());
                try {
                    ms.execute(ctr);
                } catch (ExecutionException ex) {
                    fail();
                }
                assertEquals(false, ctr.isSuccessSet());
                assertEquals(false, ctr.isSuccess());
                assertEquals(0, ctr.getNumberOfInstructions());
            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                ScriptProof sp = new ScriptProof("RETURN TRUE", 256);
                MASTstatement ms = new MASTstatement(new ConstantExpression(new HEXValue(sp.getFinalHash())));

                Witness w = new Witness();
                w.addScript(sp);
                Contract ctr = new Contract("", "", w, new Transaction(), new ArrayList<>());
                try {
                    ms.execute(ctr);
                } catch (ExecutionException ex) {
                    fail();
                }
                assertEquals(true, ctr.isSuccessSet());
                assertEquals(true, ctr.isSuccess());
                assertEquals(1, ctr.getNumberOfInstructions());
            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                ScriptProof sp = new ScriptProof("RETURN FALSE", 256);
                MASTstatement ms = new MASTstatement(new ConstantExpression(new HEXValue(sp.getFinalHash())));

                Witness w = new Witness();
                w.addScript(sp);
                Contract ctr = new Contract("", "", w, new Transaction(), new ArrayList<>());
                try {
                    ms.execute(ctr);
                } catch (ExecutionException ex) {
                    fail();
                }
                assertEquals(true, ctr.isSuccessSet());
                assertEquals(false, ctr.isSuccess());
                assertEquals(1, ctr.getNumberOfInstructions());
            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                ScriptProof sp = new ScriptProof("Hello World", 256);
                MASTstatement ms = new MASTstatement(new ConstantExpression(new HEXValue(sp.getFinalHash())));

                Witness w = new Witness();
                w.addScript(sp);
                Contract ctr = new Contract("", "", w, new Transaction(), new ArrayList<>());
                assertThrows(ExecutionException.class, () -> {
                    ms.execute(ctr);
                });
                assertEquals(false, ctr.isSuccessSet());
                assertEquals(false, ctr.isSuccess());
                assertEquals(0, ctr.getNumberOfInstructions());
            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                ScriptProof sp = new ScriptProof("RETURN TRUE", 256);
                MASTstatement ms = new MASTstatement(new ConstantExpression(new HEXValue(sp.getFinalHash())));

                Witness w = new Witness();
                Contract ctr = new Contract("", "", w, new Transaction(), new ArrayList<>());
                assertThrows(ExecutionException.class, () -> {
                    ms.execute(ctr);
                });
                assertEquals(false, ctr.isSuccessSet());
                assertEquals(false, ctr.isSuccess());
                assertEquals(0, ctr.getNumberOfInstructions());
            } catch (Exception e) {
                fail();
            }
        }
    }
}
