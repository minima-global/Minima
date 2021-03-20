package org.minima.tests.kissvm.statements;

import org.minima.kissvm.statements.StatementBlock;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.BooleanExpression;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.OperatorExpression;
import org.minima.kissvm.expressions.VariableExpression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.statements.commands.LETstatement;
import org.minima.kissvm.statements.commands.RETURNstatement;
import org.minima.kissvm.tokens.Token;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

public class StatementBlockTests {

    @Test
    public void testConstructors() {
        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();

            try {
                StatementBlock sb = new StatementBlock(Stats);
            } catch (Exception e) {
                fail();
            }
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();
            ConstantExpression cet = new ConstantExpression(new BooleanValue(true));
            Stats.add(new RETURNstatement(cet));

            try {
                StatementBlock sb = new StatementBlock(Stats);
            } catch (Exception e) {
                fail();
            }
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();
            ConstantExpression cet = new ConstantExpression(new BooleanValue(true));
            ConstantExpression cef = new ConstantExpression(new BooleanValue(true));
            Stats.add(new RETURNstatement(cet));
            Stats.add(new RETURNstatement(cef));
            Stats.add(new RETURNstatement(cet));
            Stats.add(new RETURNstatement(cef));

            try {
                StatementBlock sb = new StatementBlock(Stats);
            } catch (Exception e) {
                fail();
            }

        }
//        //Parse the tokens
//        try {
//            //Tokenize the script
//            List<Token> tokens = Token.tokenize(mRamScript);
//
//            int count = 0;
//            for (Token tok : tokens) {
//                traceLog((count++) + ") Token : [" + tok.getTokenTypeString() + "] " + tok.getToken());
//            }
//
//            //Convert this list of Tokens into a list of Statements
//            mBlock = StatementParser.parseTokens(tokens);
//
//            traceLog("Script token parse OK.");
//            mParseOK = true;
//
//        } catch (Exception e) {
//            mException = true;
//            mExceptionString = e.toString();
//
//            traceLog("PARSE ERROR : " + mExceptionString);
//        }
    }

    @Test
    public void testExecution() {
        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();
            StatementBlock sb = new StatementBlock(Stats);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                sb.run(ctr);
            } catch (ExecutionException ex) {
                fail();
            }
            assertEquals(false, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();
            ConstantExpression cet = new ConstantExpression(new BooleanValue(true));
            Stats.add(new RETURNstatement(cet));
            StatementBlock sb = new StatementBlock(Stats);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                sb.run(ctr);
            } catch (ExecutionException e) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();
            ConstantExpression cef = new ConstantExpression(new BooleanValue(false));
            Stats.add(new RETURNstatement(cef));
            StatementBlock sb = new StatementBlock(Stats);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                sb.run(ctr);
            } catch (ExecutionException e) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();

            Stats.add(new LETstatement("a",
                    new OperatorExpression(
                            new ConstantExpression(new NumberValue(0)),
                            new ConstantExpression(new NumberValue(0)),
                            OperatorExpression.OPERATOR_ADD)));
            Stats.add(new RETURNstatement(
                    new VariableExpression("a")));

            StatementBlock sb = new StatementBlock(Stats);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                sb.run(ctr);
            } catch (ExecutionException e) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(false, ctr.isSuccess());
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();

            Stats.add(new LETstatement("a",
                    new OperatorExpression(
                            new ConstantExpression(new NumberValue(5)),
                            new ConstantExpression(new NumberValue(5)),
                            OperatorExpression.OPERATOR_ADD)));
            Stats.add(new RETURNstatement(
                    new VariableExpression("a")));

            StatementBlock sb = new StatementBlock(Stats);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                sb.run(ctr);
            } catch (ExecutionException e) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }

        {
            ArrayList<Statement> Stats = new ArrayList<Statement>();

            Stats.add(new LETstatement("a",
                    new OperatorExpression(
                            new ConstantExpression(new NumberValue(5)),
                            new ConstantExpression(new NumberValue(5)),
                            OperatorExpression.OPERATOR_ADD)));
            Stats.add(new RETURNstatement(
                    new VariableExpression("a")));
            Stats.add(new RETURNstatement(
                    new VariableExpression("a")));

            StatementBlock sb = new StatementBlock(Stats);

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
            try {
                sb.run(ctr);
            } catch (ExecutionException e) {
                fail();
            }
            assertEquals(true, ctr.isSuccessSet());
            assertEquals(true, ctr.isSuccess());
        }
    }
}
