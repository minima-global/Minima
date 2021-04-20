package org.minima.tests.kissvm.tokens;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.tokens.Token;
import org.minima.kissvm.tokens.Tokenizer;
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
        assertTrue(Tokenizer.isVariable("a"));
        assertTrue(Tokenizer.isVariable("b"));
        assertTrue(Tokenizer.isVariable("c"));
        assertTrue(Tokenizer.isVariable("ab"));
        assertTrue(Tokenizer.isVariable("abc"));
        assertTrue(Tokenizer.isVariable("abcd"));
        assertTrue(Tokenizer.isVariable("abcde"));
        assertTrue(Tokenizer.isVariable("abcdefghijklmnopqrstuvwxyz")); // Not a variable, name longer than 16

        assertFalse(Tokenizer.isNumeric("a"));
        assertFalse(Tokenizer.isNumeric("b"));
        assertFalse(Tokenizer.isNumeric("c"));
        assertFalse(Tokenizer.isNumeric("ab"));
        assertFalse(Tokenizer.isNumeric("abc"));
        assertFalse(Tokenizer.isNumeric("abcd"));
        assertFalse(Tokenizer.isNumeric("abcde"));
        assertFalse(Tokenizer.isNumeric("abcdefghijklmnopqrstuvwxyz"));

        assertFalse(Tokenizer.isVariable("0"));
        assertFalse(Tokenizer.isVariable("-0"));
        assertFalse(Tokenizer.isVariable("1"));
        assertFalse(Tokenizer.isVariable("-1"));
        assertFalse(Tokenizer.isVariable("0.0"));
        assertFalse(Tokenizer.isVariable("123.456"));

        assertTrue(Tokenizer.isNumeric("0"));
//        assertTrue(Token.isNumeric("-0"));
        assertTrue(Tokenizer.isNumeric("1"));
//        assertTrue(Token.isNumeric("-1"));
        assertTrue(Tokenizer.isNumeric("0.0"));
        assertTrue(Tokenizer.isNumeric("123.456"));

        assertFalse(Tokenizer.isVariable("-123.-456"));
        assertFalse(Tokenizer.isVariable("A1"));
        assertFalse(Tokenizer.isVariable("A2"));
        assertFalse(Tokenizer.isVariable("A3"));
        assertFalse(Tokenizer.isVariable("1$#@45"));
        assertFalse(Tokenizer.isVariable("{}[]()"));

        assertFalse(Tokenizer.isNumeric("-123.-456"));
        assertFalse(Tokenizer.isNumeric("A1"));
        assertFalse(Tokenizer.isNumeric("A2"));
        assertFalse(Tokenizer.isNumeric("A3"));
        assertFalse(Tokenizer.isNumeric("1$#@45"));
        assertFalse(Tokenizer.isNumeric("{}[]()"));

        assertFalse(Tokenizer.isVariable("+123"));
        assertFalse(Tokenizer.isVariable("+123.456"));
        assertFalse(Tokenizer.isVariable(".456"));
        assertFalse(Tokenizer.isVariable("-.456"));
        assertFalse(Tokenizer.isVariable("+.456"));

        assertFalse(Tokenizer.isNumeric("+123")); // Maybe we should allow this
        assertFalse(Tokenizer.isNumeric("+123.456")); // Maybe we should allow this
        assertFalse(Tokenizer.isNumeric(".456")); // Maybe we should allow this
        assertFalse(Tokenizer.isNumeric("-.456")); // Maybe we should allow this
        assertFalse(Tokenizer.isNumeric("+.456")); // Maybe we should allow this

        assertFalse(Tokenizer.isVariable("A"));
        assertFalse(Tokenizer.isVariable("B"));
        assertFalse(Tokenizer.isVariable("C"));
        assertFalse(Tokenizer.isVariable("AB"));
        assertFalse(Tokenizer.isVariable("ABC"));
        assertFalse(Tokenizer.isVariable("ABCD"));
        assertFalse(Tokenizer.isVariable("ABCDE"));
        assertFalse(Tokenizer.isVariable("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertFalse(Tokenizer.isVariable("A1"));
        assertFalse(Tokenizer.isVariable("A2"));
        assertFalse(Tokenizer.isVariable("A3"));
        assertFalse(Tokenizer.isVariable("1$#@45"));
        assertFalse(Tokenizer.isVariable("{}[]()"));

        assertFalse(Tokenizer.isNumeric("A"));
        assertFalse(Tokenizer.isNumeric("B"));
        assertFalse(Tokenizer.isNumeric("C"));
        assertFalse(Tokenizer.isNumeric("AB"));
        assertFalse(Tokenizer.isNumeric("ABC"));
        assertFalse(Tokenizer.isNumeric("ABCD"));
        assertFalse(Tokenizer.isNumeric("ABCDE"));
        assertFalse(Tokenizer.isNumeric("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertFalse(Tokenizer.isNumeric("A1"));
        assertFalse(Tokenizer.isNumeric("A2"));
        assertFalse(Tokenizer.isNumeric("A3"));
        assertFalse(Tokenizer.isNumeric("1$#@45"));
        assertFalse(Tokenizer.isNumeric("{}[]()"));
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
            } catch (MinimaParseException ex) {
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
            } catch (MinimaParseException ex) {
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
            } catch (MinimaParseException ex) {
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
            } catch (MinimaParseException ex) {
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

            } catch (MinimaParseException ex) {
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

            } catch (MinimaParseException ex) {
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
            } catch (MinimaParseException ex) {
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
            } catch (MinimaParseException ex) {
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

            } catch (MinimaParseException ex) {
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

            } catch (MinimaParseException ex) {
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
