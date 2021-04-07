package org.minima.tests.kissvm.tokens;

import org.minima.kissvm.tokens.Token;
import org.minima.kissvm.tokens.Tokenizer;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.exceptions.SyntaxException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.minima.objects.base.MiniData;

public class TokenTests {

    @Test
    public void testConstructors() {
        for (String s : Tokenizer.TOKENS_COMMAND) {
            Token t = new Token(Token.TOKEN_COMMAND, s);
            assertEquals(Token.TOKEN_COMMAND, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("COMMAND", t.getTokenTypeString());
        }

        List<String> AllFunctions = new ArrayList<>();
        for (MinimaFunction f : MinimaFunction.ALL_FUNCTIONS) {
            AllFunctions.add(f.getName());
        }
        for (String s : AllFunctions) {
            Token t = new Token(Token.TOKEN_FUNCTIION, s);
            assertEquals(Token.TOKEN_FUNCTIION, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("FUNCTION", t.getTokenTypeString());
        }

        for (String s : Tokenizer.TOKENS_NUMBER_OPERATOR) {
            Token t = new Token(Token.TOKEN_OPERATOR, s);
            assertEquals(Token.TOKEN_OPERATOR, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("OPERATOR", t.getTokenTypeString());
        }
        
        for (String s : Tokenizer.TOKENS_BOOLEAN_OPERATOR) {
            Token t = new Token(Token.TOKEN_OPERATOR, s);
            assertEquals(Token.TOKEN_OPERATOR, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("OPERATOR", t.getTokenTypeString());
        }

        {
            String s = "0x12345678";
            Token t = new Token(Token.TOKEN_VALUE, s);
            assertEquals(Token.TOKEN_VALUE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VALUE", t.getTokenTypeString());
        }

        {
            String s = ":123456";
            Token t = new Token(Token.TOKEN_VALUE, s);
            assertEquals(Token.TOKEN_VALUE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VALUE", t.getTokenTypeString());
        }

        {
            String s = "abcdefgh";
            Token t = new Token(Token.TOKEN_VARIABLE, s);
            assertEquals(Token.TOKEN_VARIABLE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VARIABLE", t.getTokenTypeString());
        }

        {
            String s = "ijklmnop";
            Token t = new Token(Token.TOKEN_VARIABLE, s);
            assertEquals(Token.TOKEN_VARIABLE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VARIABLE", t.getTokenTypeString());
        }

        {
            String s = "@BLKNUM";
            Token t = new Token(Token.TOKEN_GLOBAL, s);
            assertEquals(Token.TOKEN_GLOBAL, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("GLOBAL", t.getTokenTypeString());
        }

        {
            String s = "@TOKENSCRIPT";
            Token t = new Token(Token.TOKEN_GLOBAL, s);
            assertEquals(Token.TOKEN_GLOBAL, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("GLOBAL", t.getTokenTypeString());
        }

        {
            String s = "(";
            Token t = new Token(Token.TOKEN_OPENBRACKET, s);
            assertEquals(Token.TOKEN_OPENBRACKET, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("OPENBRACKET", t.getTokenTypeString());
        }

        {
            String s = ")";
            Token t = new Token(Token.TOKEN_CLOSEBRACKET, s);
            assertEquals(Token.TOKEN_CLOSEBRACKET, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("CLOSEBRACKET", t.getTokenTypeString());
        }

        {
            String s = "TRUE";
            Token t = new Token(Token.TOKEN_TRUE, s);
            assertEquals(Token.TOKEN_TRUE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("TRUE", t.getTokenTypeString());
        }

        {
            String s = "FALSE";
            Token t = new Token(Token.TOKEN_FALSE, s);
            assertEquals(Token.TOKEN_FALSE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("FALSE", t.getTokenTypeString());
        }
    }

    @Test
    public void testInvalidConstructors() {
        { // Should we allow this
            String s = "whatever";
            Token t = new Token(99, s);
            assertEquals(99, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("null", t.getTokenTypeString());
        }
    }

    @Test
    public void testCheckers() {
        assertTrue(Token.isVariable("a"));
        assertTrue(Token.isVariable("b"));
        assertTrue(Token.isVariable("c"));
        assertTrue(Token.isVariable("ab"));
        assertTrue(Token.isVariable("abc"));
        assertTrue(Token.isVariable("abcd"));
        assertTrue(Token.isVariable("abcde"));
        assertTrue(Token.isVariable("abcdefghijklmnopqrstuvwxyz")); // Not a variable, name longer than 16

        assertFalse(Token.isNumeric("a"));
        assertFalse(Token.isNumeric("b"));
        assertFalse(Token.isNumeric("c"));
        assertFalse(Token.isNumeric("ab"));
        assertFalse(Token.isNumeric("abc"));
        assertFalse(Token.isNumeric("abcd"));
        assertFalse(Token.isNumeric("abcde"));
        assertFalse(Token.isNumeric("abcdefghijklmnopqrstuvwxyz"));

        assertFalse(Token.isVariable("0"));
        assertFalse(Token.isVariable("-0"));
        assertFalse(Token.isVariable("1"));
        assertFalse(Token.isVariable("-1"));
        assertFalse(Token.isVariable("0.0"));
        assertFalse(Token.isVariable("123.456"));

        assertTrue(Token.isNumeric("0"));
//        assertTrue(Token.isNumeric("-0"));
        assertTrue(Token.isNumeric("1"));
//        assertTrue(Token.isNumeric("-1"));
        assertTrue(Token.isNumeric("0.0"));
        assertTrue(Token.isNumeric("123.456"));

        assertFalse(Token.isVariable("-123.-456"));
        assertFalse(Token.isVariable("A1"));
        assertFalse(Token.isVariable("A2"));
        assertFalse(Token.isVariable("A3"));
        assertFalse(Token.isVariable("1$#@45"));
        assertFalse(Token.isVariable("{}[]()"));

        assertFalse(Token.isNumeric("-123.-456"));
        assertFalse(Token.isNumeric("A1"));
        assertFalse(Token.isNumeric("A2"));
        assertFalse(Token.isNumeric("A3"));
        assertFalse(Token.isNumeric("1$#@45"));
        assertFalse(Token.isNumeric("{}[]()"));

        assertFalse(Token.isVariable("+123"));
        assertFalse(Token.isVariable("+123.456"));
        assertFalse(Token.isVariable(".456"));
        assertFalse(Token.isVariable("-.456"));
        assertFalse(Token.isVariable("+.456"));

        assertFalse(Token.isNumeric("+123")); // Maybe we should allow this
        assertFalse(Token.isNumeric("+123.456")); // Maybe we should allow this
        assertFalse(Token.isNumeric(".456")); // Maybe we should allow this
        assertFalse(Token.isNumeric("-.456")); // Maybe we should allow this
        assertFalse(Token.isNumeric("+.456")); // Maybe we should allow this

        assertFalse(Token.isVariable("A"));
        assertFalse(Token.isVariable("B"));
        assertFalse(Token.isVariable("C"));
        assertFalse(Token.isVariable("AB"));
        assertFalse(Token.isVariable("ABC"));
        assertFalse(Token.isVariable("ABCD"));
        assertFalse(Token.isVariable("ABCDE"));
        assertFalse(Token.isVariable("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertFalse(Token.isVariable("A1"));
        assertFalse(Token.isVariable("A2"));
        assertFalse(Token.isVariable("A3"));
        assertFalse(Token.isVariable("1$#@45"));
        assertFalse(Token.isVariable("{}[]()"));

        assertFalse(Token.isNumeric("A"));
        assertFalse(Token.isNumeric("B"));
        assertFalse(Token.isNumeric("C"));
        assertFalse(Token.isNumeric("AB"));
        assertFalse(Token.isNumeric("ABC"));
        assertFalse(Token.isNumeric("ABCD"));
        assertFalse(Token.isNumeric("ABCDE"));
        assertFalse(Token.isNumeric("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertFalse(Token.isNumeric("A1"));
        assertFalse(Token.isNumeric("A2"));
        assertFalse(Token.isNumeric("A3"));
        assertFalse(Token.isNumeric("1$#@45"));
        assertFalse(Token.isNumeric("{}[]()"));
    }

    @Test
    public void testTokenize() {
        {
            String Script = "";
            for (String s : Tokenizer.TOKENS_COMMAND) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Tokenizer.TOKENS_COMMAND.length; i++) {
                    assertEquals(Token.TOKEN_COMMAND, tokens.get(i).getTokenType());
                    assertEquals(Tokenizer.TOKENS_COMMAND[i], tokens.get(i).getToken());
                }
            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "";
            for (MinimaFunction f : MinimaFunction.ALL_FUNCTIONS) {
                Script = Script + f.getName() + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < MinimaFunction.ALL_FUNCTIONS.length; i++) {
                    assertEquals(Token.TOKEN_FUNCTIION, tokens.get(i).getTokenType());
                    assertEquals(MinimaFunction.ALL_FUNCTIONS[i].getName(), tokens.get(i).getToken());
                }
            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "";
            for (String s : Tokenizer.TOKENS_NUMBER_OPERATOR) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Tokenizer.TOKENS_NUMBER_OPERATOR.length; i++) {
                    assertEquals(Token.TOKEN_OPERATOR, tokens.get(i).getTokenType());
                    assertEquals(Tokenizer.TOKENS_NUMBER_OPERATOR[i], tokens.get(i).getToken());
                }
            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "";
            String[] Values = {"[ abc ]", "[ def ]", "[ ghi ]", "[ abc [ def ] ghi ]"};
            for (String s : Values) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(Token.TOKEN_VALUE, tokens.get(i).getTokenType());
                    assertEquals(Values[i], tokens.get(i).getToken());
                }
            } catch (SyntaxException ex) {
                fail();
            }
        }

//        {
//            String Script = ":a";
//            assertThrows(MinimaParseException.class, () -> {
//                Token.tokenize(Script);
//            });
//        }
//
//        {
//            String Script = ":a1";
//            assertThrows(MinimaParseException.class, () -> {
//                Token.tokenize(Script);
//            });
//        }
//
//        {
//            String Script = ":1a";
//            assertThrows(MinimaParseException.class, () -> {
//                Token.tokenize(Script);
//            });
//        }
//
//        {
//            String Script = ":a1b";
//            assertThrows(MinimaParseException.class, () -> {
//                Token.tokenize(Script);
//            });
//        }
//
//        {
//            String Script = ":1a1b";
//            assertThrows(MinimaParseException.class, () -> {
//                Token.tokenize(Script);
//            });
//        }
//
//        {
//            String Script = ":1a1b1";
//            assertThrows(MinimaParseException.class, () -> {
//                Token.tokenize(Script);
//            });
//        }

        {
            String Script = "";
            String[] Values = {"0x1", "0x12", "0x123", "0x1234", "0x12345", "0x123456", "0x1234567", "0x12345678", "0x1234567890123456789"};
            for (String s : Values) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(Token.TOKEN_VALUE, tokens.get(i).getTokenType());
                    assertEquals(Values[i], tokens.get(i).getToken());
                }

            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "";
            String[] Values = {"0xA", "0xAB", "0x1AB", "0x1A2B",
                MiniData.getRandomData(4).toString(),
                MiniData.getRandomData(8).toString(),
                MiniData.getRandomData(16).toString(),
                MiniData.getRandomData(32).toString(),
                MiniData.getRandomData(64).toString()};
            for (String s : Values) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(Token.TOKEN_VALUE, tokens.get(i).getTokenType());
                    assertEquals(Values[i], tokens.get(i).getToken());
                }

            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "( ) ) ( ( ) ) ( ( ) ) ( ( ) ) ( ( )";
            try {
                List<Token> tokens = Token.tokenize(Script);
                int i = 0;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "TRUE FALSE FALSE TRUE TRUE FALSE FALSE TRUE TRUE FALSE FALSE TRUE TRUE FALSE FALSE TRUE TRUE FALSE";
            try {
                List<Token> tokens = Token.tokenize(Script);
                int i = 0;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(Token.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "";
            String[] Values = {"@ADDRESS", "@AMOUNT", "@COINID", "@INBLKNUM", "@INPUT", "@BLKNUM", "@BLKTIME", "@SCRIPT", "@TOKENID", "@TOKENSCRIPT", "@TOKENTOTAL", "@TOTIN", "@TOTOUT"};
            for (String s : Values) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(Token.TOKEN_GLOBAL, tokens.get(i).getTokenType());
                    assertEquals(Values[i], tokens.get(i).getToken());
                }

            } catch (SyntaxException ex) {
                fail();
            }
        }

        {
            String Script = "";
            String[] Values = {"a", "bc", "def", "ghij", "klmno", "pqrstu", "vwxyzab", "cdefghij", "klmnopqrs", "tuvwxyzabc", "defghijklmno", "pqrstuvwxyzab", "cdefghijklmnop", "qrstuvwxyzabcdef"};
            for (String s : Values) {
                Script = Script + s + " ";
            }
            try {
                List<Token> tokens = Token.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(Token.TOKEN_VARIABLE, tokens.get(i).getTokenType());
                    assertEquals(Values[i], tokens.get(i).getToken());
                }

            } catch (SyntaxException ex) {
                fail();
            }
        }

//        {
//            String Script = "abcdefghijklmnopqrstuvwxyz";
//            assertThrows(MinimaParseException.class, () -> { // should throw this
//                Token.tokenize(Script);
//            });
//        }
    }
}
