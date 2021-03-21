package org.minima.tests.kissvm.statements;

import org.minima.kissvm.statements.StatementParser;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.BooleanExpression;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.OperatorExpression;
import org.minima.kissvm.expressions.VariableExpression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
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

public class StatementParserTests {

    @Test
    public void testConstructors() {
        {
            try {
                String Script = "";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }
    }

    @Test
    public void testParsingLETArrays() {
        {
            try {
                String Script = "LET (A) = TRUE LET (B) = FALSE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                String Script = "LET (A = TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

        {
            //assertThrows(MinimaParseException.class, () -> {
            try {
                String Script = "LET (A * ) = TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
            //});
        }

        {
            //assertThrows(MinimaParseException.class, () -> {
            try {
                String Script = "LET ( ) = 5";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
            //});
        }

        {
            try {
                String Script = "LET (A B C D E) = TRUE LET (B C D E F) = FALSE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try {
                String Script = "LET (TRUE 1 0x123456 D BOOL(1)) = TRUE LET (FALSE -1 0xABCDEF D SCRIPT(0xABCDEF)) = FALSE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }
    }

    @Test
    public void testParsingLET() {
        {
            try {
                String Script = "LET A = TRUE LET B = FALSE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try {
                String Script = "LET A = 0x12345678 LET B = 0x87654321";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try {
                String Script = "LET A = -1 LET B = 0 LET C = 1 LET D = 9999999999 LET E = -0.000005";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "LET"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "a"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "+"));
                tokens.add(new Token(Token.TOKEN_VALUE, "5"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

        {
            try {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "LET"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "a"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "="));
                tokens.add(new Token(Token.TOKEN_VALUE, "5"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "LET"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "a"));
                tokens.add(new Token(Token.TOKEN_OPERATOR, "+"));
                tokens.add(new Token(Token.TOKEN_VALUE, "5"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "LET"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "a"));
                tokens.add(new Token(Token.TOKEN_OPERATOR, "="));
                tokens.add(new Token(Token.TOKEN_VALUE, "5"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "LET"));
                tokens.add(new Token(Token.TOKEN_OPERATOR, "="));
                tokens.add(new Token(Token.TOKEN_VALUE, "5"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }
    }

    @Test
    public void testParsingEXEC() {
        {
            try {
                String Script = "EXEC [RETURN TRUE]";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "EXEC [RETURN TRUE] + 543 RETURN TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "EXEC RETURN";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }
    }

    @Test
    public void testParsingMAST() {
        {
            try {
                String Script = "MAST 1 + 5";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "MAST TRUE + 543";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "MAST RETURN TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }
    }

    @Test
    public void testParsingASSERT() {
        {
            try {
                String Script = "ASSERT TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try {
                String Script = "ASSERT FALSE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "ASSERT RETURN TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "ASSERT RETURN A + @BLKNUM";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "ASSERT [RETURN TRUE] + 543 RETURN TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }
    }

    @Test
    public void testParsingRETURN() {
        {
            try {
                String Script = "RETURN 1 + 5";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "RETURN TRUE + 543";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }

        {
            try { // Strange
                String Script = "RETURN MAST 1";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
                fail();
            } catch (Exception ex) {
                fail();
            }
        }
    }

    @Test
    public void testParsingException() {
        {
            assertThrows(MinimaParseException.class, () -> {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "HELLO"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }
    }
}
