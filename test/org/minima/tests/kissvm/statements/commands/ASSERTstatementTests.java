package org.minima.tests.kissvm.statements.commands;

import org.minima.kissvm.statements.commands.ASSERTstatement;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.BooleanExpression;
import org.minima.kissvm.expressions.ConstantExpression;
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

public class ASSERTstatementTests {

    @Test
    public void testConstructors() {
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new BooleanValue(true)));
            assertEquals("ASSERT TRUE", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new BooleanValue(false)));
            assertEquals("ASSERT FALSE", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new HEXValue("")));
            assertEquals("ASSERT 0x", as.toString()); // Wrong???
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new HEXValue("0x00")));
            assertEquals("ASSERT 0x00", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new HEXValue("0x12345678")));
            assertEquals("ASSERT 0x12345678", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new HEXValue("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")));
            assertEquals("ASSERT 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new NumberValue(-1)));
            assertEquals("ASSERT -1", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new NumberValue(0)));
            assertEquals("ASSERT 0", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new NumberValue(1)));
            assertEquals("ASSERT 1", as.toString());
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new ScriptValue("")));
            assertEquals("ASSERT ", as.toString()); // Wrong???
        }
        {
            ASSERTstatement as = new ASSERTstatement(new ConstantExpression(new ScriptValue("Hello World")));
            assertEquals("ASSERT hello world", as.toString());
        }
    }

    @Test
    public void testExecution() {
        {
            ConstantExpression ce = new ConstantExpression(new BooleanValue(true));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new BooleanValue(false));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue(""));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue("0x00"));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue("0x12345678"));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new NumberValue(-1));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new NumberValue(0));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new NumberValue(1));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new ScriptValue(""));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new ScriptValue("Hello World"));
            ASSERTstatement as = new ASSERTstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                as.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
    }

}
