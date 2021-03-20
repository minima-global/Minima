package org.minima.tests.kissvm.statements.commands;

import org.minima.kissvm.statements.commands.RETURNstatement;

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

public class RETURNstatementTests {

    @Test
    public void testConstructors() {
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new BooleanValue(true)));
            assertEquals("RETURN TRUE", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new BooleanValue(false)));
            assertEquals("RETURN FALSE", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new HEXValue("")));
            assertEquals("RETURN 0x", rs.toString()); // Wrong???
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new HEXValue("0x00")));
            assertEquals("RETURN 0x00", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new HEXValue("0x12345678")));
            assertEquals("RETURN 0x12345678", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new HEXValue("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")));
            assertEquals("RETURN 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new NumberValue(-1)));
            assertEquals("RETURN -1", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new NumberValue(0)));
            assertEquals("RETURN 0", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new NumberValue(1)));
            assertEquals("RETURN 1", rs.toString());
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new ScriptValue("")));
            assertEquals("RETURN ", rs.toString()); // Wrong???
        }
        {
            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new ScriptValue("Hello World")));
            assertEquals("RETURN hello world", rs.toString());
        }
    }

    @Test
    public void testExecution() {
        {
            ConstantExpression ce = new ConstantExpression(new BooleanValue(true));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new BooleanValue(false));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue(""));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue("0x00"));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue("0x12345678"));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new HEXValue("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new NumberValue(-1));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new NumberValue(0));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new NumberValue(1));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new ScriptValue(""));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }
        {
            ConstantExpression ce = new ConstantExpression(new ScriptValue("Hello World"));
            RETURNstatement rs = new RETURNstatement(ce);
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                rs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
    }

}
