package org.minima.tests.kissvm.statements.commands;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.BooleanExpression;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.OperatorExpression;
import org.minima.kissvm.expressions.VariableExpression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.commands.LETstatement;
import org.minima.kissvm.statements.commands.RETURNstatement;
import org.minima.kissvm.statements.commands.WHILEstatement;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

public class WHILEstatementTests {

    @Test
    public void testConstructors() {
        BooleanExpression WhileCheck = new BooleanExpression(
                new VariableExpression("a"),
                new ConstantExpression(new NumberValue(5)),
                BooleanExpression.BOOLEAN_LT);

        OperatorExpression oe1 = new OperatorExpression(
                new VariableExpression("a"),
                new ConstantExpression(new NumberValue(1)),
                OperatorExpression.OPERATOR_ADD);

        LETstatement ls2 = new LETstatement("a", oe1);

        ArrayList<Statement> WhileCodeBlockAL = new ArrayList<Statement>();
        WhileCodeBlockAL.add(ls2);

        StatementBlock WhileCodeBlock = new StatementBlock(WhileCodeBlockAL);

        WHILEstatement ws = new WHILEstatement(WhileCheck, WhileCodeBlock);

        assertEquals("WHILE ( variable:a LT 5 )", ws.toString());

    }

    @Test
    public void testExecution() {
        {
            ArrayList<Statement> CodeBlockAL = new ArrayList<Statement>();

            LETstatement ls1 = new LETstatement("a", new ConstantExpression(new NumberValue(0)));

            CodeBlockAL.add(ls1);

            BooleanExpression WhileCheck = new BooleanExpression(
                    new VariableExpression("a"),
                    new ConstantExpression(new NumberValue(5)),
                    BooleanExpression.BOOLEAN_LT);

            OperatorExpression oe1 = new OperatorExpression(
                    new VariableExpression("a"),
                    new ConstantExpression(new NumberValue(1)),
                    OperatorExpression.OPERATOR_ADD);

            LETstatement ls2 = new LETstatement("a", oe1);

            ArrayList<Statement> WhileCodeBlockAL = new ArrayList<Statement>();
            WhileCodeBlockAL.add(ls2);

            StatementBlock WhileCodeBlock = new StatementBlock(WhileCodeBlockAL);

            WHILEstatement ws = new WHILEstatement(WhileCheck, WhileCodeBlock);

            CodeBlockAL.add(ws);

            StatementBlock CodeBlock = new StatementBlock(CodeBlockAL);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                CodeBlock.run(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            ArrayList<Statement> CodeBlockAL = new ArrayList<Statement>();

            LETstatement ls1 = new LETstatement("a", new ConstantExpression(new NumberValue(0)));

            CodeBlockAL.add(ls1);

            BooleanExpression WhileCheck = new BooleanExpression(
                    new VariableExpression("a"),
                    new ConstantExpression(new NumberValue(5)),
                    BooleanExpression.BOOLEAN_LT);

            OperatorExpression oe1 = new OperatorExpression(
                    new VariableExpression("a"),
                    new ConstantExpression(new NumberValue(1)),
                    OperatorExpression.OPERATOR_ADD);

            LETstatement ls2 = new LETstatement("a", oe1);

            ArrayList<Statement> WhileCodeBlockAL = new ArrayList<Statement>();
            WhileCodeBlockAL.add(ls2);

            RETURNstatement rs = new RETURNstatement(new ConstantExpression(new BooleanValue(true)));
            WhileCodeBlockAL.add(rs);

            StatementBlock WhileCodeBlock = new StatementBlock(WhileCodeBlockAL);

            WHILEstatement ws = new WHILEstatement(WhileCheck, WhileCodeBlock);

            CodeBlockAL.add(ws);

            StatementBlock CodeBlock = new StatementBlock(CodeBlockAL);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                CodeBlock.run(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

}
