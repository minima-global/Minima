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
import org.minima.kissvm.tokens.ScriptToken;
import org.minima.kissvm.tokens.ScriptTokenizer;
import org.minima.objects.base.MiniData;

public class TokenTests {

    @Test
    public void testConstructors() {
        for (String s : ScriptTokenizer.TOKENS_COMMAND) {
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_COMMAND, s);
            assertEquals(ScriptToken.TOKEN_COMMAND, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("COMMAND", t.getTokenTypeString());
        }

        List<String> AllFunctions = new ArrayList<>();
        for (MinimaFunction f : MinimaFunction.ALL_FUNCTIONS) {
            AllFunctions.add(f.getName());
        }
        for (String s : AllFunctions) {
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_FUNCTIION, s);
            assertEquals(ScriptToken.TOKEN_FUNCTIION, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("FUNCTION", t.getTokenTypeString());
        }

        for (String s : ScriptTokenizer.TOKENS_NUMBER_OPERATOR) {
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_OPERATOR, s);
            assertEquals(ScriptToken.TOKEN_OPERATOR, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("OPERATOR", t.getTokenTypeString());
        }
        
        for (String s : ScriptTokenizer.TOKENS_BOOLEAN_OPERATOR) {
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_OPERATOR, s);
            assertEquals(ScriptToken.TOKEN_OPERATOR, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("OPERATOR", t.getTokenTypeString());
        }

        {
            String s = "0x12345678";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_VALUE, s);
            assertEquals(ScriptToken.TOKEN_VALUE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VALUE", t.getTokenTypeString());
        }

        {
            String s = ":123456";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_VALUE, s);
            assertEquals(ScriptToken.TOKEN_VALUE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VALUE", t.getTokenTypeString());
        }

        {
            String s = "abcdefgh";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_VARIABLE, s);
            assertEquals(ScriptToken.TOKEN_VARIABLE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VARIABLE", t.getTokenTypeString());
        }

        {
            String s = "ijklmnop";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_VARIABLE, s);
            assertEquals(ScriptToken.TOKEN_VARIABLE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("VARIABLE", t.getTokenTypeString());
        }

        {
            String s = "@BLKNUM";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_GLOBAL, s);
            assertEquals(ScriptToken.TOKEN_GLOBAL, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("GLOBAL", t.getTokenTypeString());
        }

        {
            String s = "@TOKENSCRIPT";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_GLOBAL, s);
            assertEquals(ScriptToken.TOKEN_GLOBAL, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("GLOBAL", t.getTokenTypeString());
        }

        {
            String s = "(";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_OPENBRACKET, s);
            assertEquals(ScriptToken.TOKEN_OPENBRACKET, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("OPENBRACKET", t.getTokenTypeString());
        }

        {
            String s = ")";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_CLOSEBRACKET, s);
            assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("CLOSEBRACKET", t.getTokenTypeString());
        }

        {
            String s = "TRUE";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_TRUE, s);
            assertEquals(ScriptToken.TOKEN_TRUE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("TRUE", t.getTokenTypeString());
        }

        {
            String s = "FALSE";
            ScriptToken t = new ScriptToken(ScriptToken.TOKEN_FALSE, s);
            assertEquals(ScriptToken.TOKEN_FALSE, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("FALSE", t.getTokenTypeString());
        }
    }

    @Test
    public void testInvalidConstructors() {
        { // Should we allow this
            String s = "whatever";
            ScriptToken t = new ScriptToken(99, s);
            assertEquals(99, t.getTokenType());
            assertEquals(s, t.getToken());
            assertEquals("null", t.getTokenTypeString());
        }
    }

    @Test
    public void testCheckers() {
        assertTrue(ScriptTokenizer.isVariable("a"));
        assertTrue(ScriptTokenizer.isVariable("b"));
        assertTrue(ScriptTokenizer.isVariable("c"));
        assertTrue(ScriptTokenizer.isVariable("ab"));
        assertTrue(ScriptTokenizer.isVariable("abc"));
        assertTrue(ScriptTokenizer.isVariable("abcd"));
        assertTrue(ScriptTokenizer.isVariable("abcde"));
        assertTrue(ScriptTokenizer.isVariable("abcdefghijklmnopqrstuvwxyz")); // Not a variable, name longer than 16

        assertFalse(ScriptTokenizer.isNumeric("a"));
        assertFalse(ScriptTokenizer.isNumeric("b"));
        assertFalse(ScriptTokenizer.isNumeric("c"));
        assertFalse(ScriptTokenizer.isNumeric("ab"));
        assertFalse(ScriptTokenizer.isNumeric("abc"));
        assertFalse(ScriptTokenizer.isNumeric("abcd"));
        assertFalse(ScriptTokenizer.isNumeric("abcde"));
        assertFalse(ScriptTokenizer.isNumeric("abcdefghijklmnopqrstuvwxyz"));

        assertFalse(ScriptTokenizer.isVariable("0"));
        assertFalse(ScriptTokenizer.isVariable("-0"));
        assertFalse(ScriptTokenizer.isVariable("1"));
        assertFalse(ScriptTokenizer.isVariable("-1"));
        assertFalse(ScriptTokenizer.isVariable("0.0"));
        assertFalse(ScriptTokenizer.isVariable("123.456"));

        assertTrue(ScriptTokenizer.isNumeric("0"));
//        assertTrue(Token.isNumeric("-0"));
        assertTrue(ScriptTokenizer.isNumeric("1"));
//        assertTrue(Token.isNumeric("-1"));
        assertTrue(ScriptTokenizer.isNumeric("0.0"));
        assertTrue(ScriptTokenizer.isNumeric("123.456"));

        assertFalse(ScriptTokenizer.isVariable("-123.-456"));
        assertFalse(ScriptTokenizer.isVariable("A1"));
        assertFalse(ScriptTokenizer.isVariable("A2"));
        assertFalse(ScriptTokenizer.isVariable("A3"));
        assertFalse(ScriptTokenizer.isVariable("1$#@45"));
        assertFalse(ScriptTokenizer.isVariable("{}[]()"));

        assertFalse(ScriptTokenizer.isNumeric("-123.-456"));
        assertFalse(ScriptTokenizer.isNumeric("A1"));
        assertFalse(ScriptTokenizer.isNumeric("A2"));
        assertFalse(ScriptTokenizer.isNumeric("A3"));
        assertFalse(ScriptTokenizer.isNumeric("1$#@45"));
        assertFalse(ScriptTokenizer.isNumeric("{}[]()"));

        assertFalse(ScriptTokenizer.isVariable("+123"));
        assertFalse(ScriptTokenizer.isVariable("+123.456"));
        assertFalse(ScriptTokenizer.isVariable(".456"));
        assertFalse(ScriptTokenizer.isVariable("-.456"));
        assertFalse(ScriptTokenizer.isVariable("+.456"));

        assertFalse(ScriptTokenizer.isNumeric("+123")); // Maybe we should allow this
        assertFalse(ScriptTokenizer.isNumeric("+123.456")); // Maybe we should allow this
        assertFalse(ScriptTokenizer.isNumeric(".456")); // Maybe we should allow this
        assertFalse(ScriptTokenizer.isNumeric("-.456")); // Maybe we should allow this
        assertFalse(ScriptTokenizer.isNumeric("+.456")); // Maybe we should allow this

        assertFalse(ScriptTokenizer.isVariable("A"));
        assertFalse(ScriptTokenizer.isVariable("B"));
        assertFalse(ScriptTokenizer.isVariable("C"));
        assertFalse(ScriptTokenizer.isVariable("AB"));
        assertFalse(ScriptTokenizer.isVariable("ABC"));
        assertFalse(ScriptTokenizer.isVariable("ABCD"));
        assertFalse(ScriptTokenizer.isVariable("ABCDE"));
        assertFalse(ScriptTokenizer.isVariable("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertFalse(ScriptTokenizer.isVariable("A1"));
        assertFalse(ScriptTokenizer.isVariable("A2"));
        assertFalse(ScriptTokenizer.isVariable("A3"));
        assertFalse(ScriptTokenizer.isVariable("1$#@45"));
        assertFalse(ScriptTokenizer.isVariable("{}[]()"));

        assertFalse(ScriptTokenizer.isNumeric("A"));
        assertFalse(ScriptTokenizer.isNumeric("B"));
        assertFalse(ScriptTokenizer.isNumeric("C"));
        assertFalse(ScriptTokenizer.isNumeric("AB"));
        assertFalse(ScriptTokenizer.isNumeric("ABC"));
        assertFalse(ScriptTokenizer.isNumeric("ABCD"));
        assertFalse(ScriptTokenizer.isNumeric("ABCDE"));
        assertFalse(ScriptTokenizer.isNumeric("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertFalse(ScriptTokenizer.isNumeric("A1"));
        assertFalse(ScriptTokenizer.isNumeric("A2"));
        assertFalse(ScriptTokenizer.isNumeric("A3"));
        assertFalse(ScriptTokenizer.isNumeric("1$#@45"));
        assertFalse(ScriptTokenizer.isNumeric("{}[]()"));
    }

    @Test
    public void testTokenize() {
        {
            String Script = "";
            for (String s : ScriptTokenizer.TOKENS_COMMAND) {
                Script = Script + s + " ";
            }
            try {
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < ScriptTokenizer.TOKENS_COMMAND.length; i++) {
                    assertEquals(ScriptToken.TOKEN_COMMAND, tokens.get(i).getTokenType());
                    assertEquals(ScriptTokenizer.TOKENS_COMMAND[i], tokens.get(i).getToken());
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
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < MinimaFunction.ALL_FUNCTIONS.length; i++) {
                    assertEquals(ScriptToken.TOKEN_FUNCTIION, tokens.get(i).getTokenType());
                    assertEquals(MinimaFunction.ALL_FUNCTIONS[i].getName(), tokens.get(i).getToken());
                }
            } catch (MinimaParseException ex) {
                fail();
            }
        }

        {
            String Script = "";
            for (String s : ScriptTokenizer.TOKENS_NUMBER_OPERATOR) {
                Script = Script + s + " ";
            }
            try {
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < ScriptTokenizer.TOKENS_NUMBER_OPERATOR.length; i++) {
                    assertEquals(ScriptToken.TOKEN_OPERATOR, tokens.get(i).getTokenType());
                    assertEquals(ScriptTokenizer.TOKENS_NUMBER_OPERATOR[i], tokens.get(i).getToken());
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
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(ScriptToken.TOKEN_VALUE, tokens.get(i).getTokenType());
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
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(ScriptToken.TOKEN_VALUE, tokens.get(i).getTokenType());
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
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(ScriptToken.TOKEN_VALUE, tokens.get(i).getTokenType());
                    assertEquals(Values[i], tokens.get(i).getToken());
                }

            } catch (MinimaParseException ex) {
                fail();
            }
        }

        {
            String Script = "( ) ) ( ( ) ) ( ( ) ) ( ( ) ) ( ( )";
            try {
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                int i = 0;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_OPENBRACKET, tokens.get(i).getTokenType());
                assertEquals("(", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_CLOSEBRACKET, tokens.get(i).getTokenType());
                assertEquals(")", tokens.get(i).getToken());
                i++;
            } catch (MinimaParseException ex) {
                fail();
            }
        }

        {
            String Script = "TRUE FALSE FALSE TRUE TRUE FALSE FALSE TRUE TRUE FALSE FALSE TRUE TRUE FALSE FALSE TRUE TRUE FALSE";
            try {
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                int i = 0;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
                assertEquals("FALSE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_TRUE, tokens.get(i).getTokenType());
                assertEquals("TRUE", tokens.get(i).getToken());
                i++;
                assertEquals(ScriptToken.TOKEN_FALSE, tokens.get(i).getTokenType());
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
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(ScriptToken.TOKEN_GLOBAL, tokens.get(i).getTokenType());
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
                List<ScriptToken> tokens = ScriptToken.tokenize(Script);
                for (int i = 0; i < Values.length; i++) {
                    assertEquals(ScriptToken.TOKEN_VARIABLE, tokens.get(i).getTokenType());
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
