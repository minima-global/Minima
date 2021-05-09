package org.minima.tests.kissvm.statements.commands;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.commands.IFstatement;
import org.minima.kissvm.statements.commands.RETURNstatement;
import org.minima.kissvm.values.BooleanValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

public class IFstatementTests {

    @Test
    public void testConstructors() {
        ConstantExpression CheckTrue = new ConstantExpression(new BooleanValue(false));
        ConstantExpression CheckFalse = new ConstantExpression(new BooleanValue(false));

        RETURNstatement ReturnTrue = new RETURNstatement(CheckTrue);
        RETURNstatement ReturnFalse = new RETURNstatement(CheckFalse);

        ArrayList<Statement> CodeBlockReturnTrueAL = new ArrayList<Statement>();
        CodeBlockReturnTrueAL.add(ReturnTrue);
        StatementBlock CodeBlockReturnTrue = new StatementBlock(CodeBlockReturnTrueAL);

        ArrayList<Statement> CodeBlockReturnFalseAL = new ArrayList<Statement>();
        CodeBlockReturnFalseAL.add(ReturnFalse);
        StatementBlock CodeBlockReturnFalse = new StatementBlock(CodeBlockReturnTrueAL);

        IFstatement ifs = new IFstatement();
        ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
        ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
        ifs.addCondition(CheckTrue, CodeBlockReturnTrue);

        assertEquals("IF FALSE, ELSEIF FALSE, ELSEIF FALSE", ifs.toString());

    }

    @Test
    public void testExecution() {
        ConstantExpression CheckTrue = new ConstantExpression(new BooleanValue(true));
        ConstantExpression CheckFalse = new ConstantExpression(new BooleanValue(false));

        RETURNstatement ReturnTrue = new RETURNstatement(CheckTrue);
        RETURNstatement ReturnFalse = new RETURNstatement(CheckFalse);

        ArrayList<Statement> CodeBlockReturnTrueAL = new ArrayList<Statement>();
        CodeBlockReturnTrueAL.add(ReturnTrue);
        StatementBlock CodeBlockReturnTrue = new StatementBlock(CodeBlockReturnTrueAL);

        ArrayList<Statement> CodeBlockReturnFalseAL = new ArrayList<Statement>();
        CodeBlockReturnFalseAL.add(ReturnFalse);
        StatementBlock CodeBlockReturnFalse = new StatementBlock(CodeBlockReturnFalseAL);

        {
            IFstatement ifs = new IFstatement();

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            //assertThrows(ExecutionException.class, () -> { // should throw this
            //    ifs.execute(ctr);
            //});

            // but does not throw
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
            assertEquals(0, ctr.getNumberOfInstructions());
        }
        {
            IFstatement ifs = new IFstatement();
            ifs.addCondition(CheckTrue, CodeBlockReturnTrue);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            IFstatement ifs = new IFstatement();
            ifs.addCondition(CheckTrue, CodeBlockReturnTrue);
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            IFstatement ifs = new IFstatement();
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
            ifs.addCondition(CheckTrue, CodeBlockReturnTrue);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            IFstatement ifs = new IFstatement();
            ifs.addCondition(CheckTrue, CodeBlockReturnTrue);
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            IFstatement ifs = new IFstatement();
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
            ifs.addCondition(CheckTrue, CodeBlockReturnTrue);
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
        {
            IFstatement ifs = new IFstatement();
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
            ifs.addCondition(CheckFalse, CodeBlockReturnFalse);
            ifs.addCondition(CheckTrue, CodeBlockReturnTrue);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                ifs.execute(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
            assertEquals(1, ctr.getNumberOfInstructions());
        }
    }
}
