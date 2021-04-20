package org.minima.tests.kissvm.statements;

import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.tokens.Token;
import org.minima.utils.MinimaLogger;

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
                String Script = "LET (a) = TRUE LET (b) = FALSE";
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
                String Script = "LET (a = TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                String Script = "LET (a * ) = TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });	
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                String Script = "LET ( ) = 5";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

        {
            try {
                String Script = "LET (a b c d e) = TRUE LET (b c d e f) = FALSE";
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
                String Script = "LET (TRUE 1 0x123456 d BOOL(1)) = TRUE LET (FALSE -1 0xABCDEF d SCRIPT(0xABCDEF)) = FALSE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            } catch (MinimaParseException ex) {
            	MinimaLogger.log(ex);
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
                String Script = "LET a = TRUE LET b = FALSE";
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
                String Script = "LET a = 0x12345678 LET b = 0x87654321";
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
                String Script = "LET a = -1 LET b = 0 LET c = 1 LET d = 9999999999 LET e = -0.000005";
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
        	assertThrows(MinimaParseException.class, () -> {
                ArrayList<Token> tokens = new ArrayList<>();
                tokens.add(new Token(Token.TOKEN_COMMAND, "LET"));
                tokens.add(new Token(Token.TOKEN_VARIABLE, "a"));
                tokens.add(new Token(Token.TOKEN_OPERATOR, "+"));
                tokens.add(new Token(Token.TOKEN_VALUE, "5"));
                StatementBlock sb = StatementParser.parseTokens(tokens);
        	});
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
        	assertThrows(MinimaParseException.class, () -> {
                String Script = "EXEC RETURN";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
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
        	assertThrows(MinimaParseException.class, () -> {
                String Script = "MAST RETURN TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }
    }

    @Test
    public void testParsingIF() {
        {
            String[] Exps = {"TRUE", "FALSE", "0x01", "0x00", "1", "0", "1.5", "0.0", "[hello world]", "[]"};
            for (String Exp : Exps) {
                try {
                    String Script = "IF " + Exp + " THEN RETURN TRUE ENDIF";
                    List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                    StatementBlock sb = StatementParser.parseTokens(tokens);
                } catch (MinimaParseException ex) {
                    fail();
                } catch (Exception ex) {
                    fail();
                }

                try {
                    String Script = "IF " + Exp + " THEN RETURN TRUE ELSE RETURN FALSE ENDIF";
                    List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                    StatementBlock sb = StatementParser.parseTokens(tokens);
                } catch (MinimaParseException ex) {
                    fail();
                } catch (Exception ex) {
                    fail();
                }

                try {
                    String Script = "IF " + Exp + " THEN RETURN TRUE ELSEIF FALSE THEN RETURN FALSE ELSE RETURN FALSE ENDIF";
                    List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                    StatementBlock sb = StatementParser.parseTokens(tokens);
                } catch (MinimaParseException ex) {
                    fail();
                } catch (Exception ex) {
                    fail();
                }

                try {
                    String Script = "IF " + Exp + " THEN RETURN TRUE ELSE IF FALSE THEN RETURN FALSE ELSE RETURN FALSE ENDIF ENDIF";
                    List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                    StatementBlock sb = StatementParser.parseTokens(tokens);
                } catch (MinimaParseException ex) {
                    fail();
                } catch (Exception ex) {
                    fail();
                }
            }
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                String Script = "IF ( ) THEN RETURN TRUE ENDIF";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }
        {
            assertThrows(MinimaParseException.class, () -> {
                String Script = "IF ( A + ) THEN RETURN TRUE ENDIF";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

        {
            assertThrows(MinimaParseException.class, () -> {
                String Script = "IF FALSE THEN RETURN TRUE HELLO FALSE THEN RETURN FALSE ELSE RETURN FALSE ENDIF";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

    }

    @Test
    public void testParsingWHILE() {
        {
            String[] Exps = {"TRUE", "FALSE", "0x01", "0x00", "1", "0", "1.5", "0.0", "[hello world]", "[]"};
            for (String Exp : Exps) {
                try {
                    String Script = "WHILE " + Exp + " DO RETURN TRUE ENDWHILE";
                    List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                    StatementBlock sb = StatementParser.parseTokens(tokens);
                } catch (MinimaParseException ex) {
                    fail();
                } catch (Exception ex) {
                    fail();
                }
            }
        }
        {
            String[] Exps = {"TRUE", "FALSE", "0x01", "0x00", "1", "0", "1.5", "0.0", "[hello world]", "[]"};
            for (String Exp : Exps) {
                try {
                    String Script = "WHILE " + Exp + " DO WHILE " + Exp + " DO RETURN TRUE ENDWHILE ENDWHILE";
                    List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                    StatementBlock sb = StatementParser.parseTokens(tokens);
                } catch (MinimaParseException ex) {
                    fail();
                } catch (Exception ex) {
                    fail();
                }
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
        	assertThrows(MinimaParseException.class, () -> {
                String Script = "ASSERT RETURN TRUE";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
        }

        {
        	assertThrows(MinimaParseException.class, () -> {
                String Script = "ASSERT RETURN A + @BLKNUM";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
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
        	assertThrows(MinimaParseException.class, () -> {
                String Script = "RETURN MAST 1";
                List<Token> tokens = Token.tokenize(Contract.cleanScript(Script));
                StatementBlock sb = StatementParser.parseTokens(tokens);
            });
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
