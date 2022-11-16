package org.minima.tests.kissvm.tokens;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.tokens.LexicalTokenizer;
import org.minima.kissvm.tokens.ScriptToken;

public class LexicalTokenizerTests {

    @Test
    public void testConstructors() {
        LexicalTokenizer lt = new LexicalTokenizer(new ArrayList<ScriptToken>());
        assertThrows(MinimaParseException.class, () -> {
            lt.getNextToken();
        });
        assertEquals(0, lt.getCurrentPosition());
        assertEquals(true, lt.checkAllTokensUsed());
        assertEquals(false, lt.hasMoreElements());

        assertThrows(MinimaParseException.class, () -> {
            lt.goBackToken(); // negative position not allowed
        });
    }

    @Test
    public void testGetters() {
        String Script = "a b c d e f g h i j k l m n o p q r s t u v w x y z";
        try {
            List<ScriptToken> tokens = ScriptToken.tokenize(Script);
            LexicalTokenizer lt = new LexicalTokenizer(tokens);

            for (int i = 0; i < tokens.size(); i++) {
                assertEquals(i, lt.getCurrentPosition());
                assertEquals(false, lt.checkAllTokensUsed());
                assertEquals(true, lt.hasMoreElements());

                ScriptToken t;
                try {
                    t = lt.getNextToken();
                    assertEquals(t.getToken(), tokens.get(i).getToken());

                    if (i == tokens.size() - 1) {
                        assertEquals(true, lt.checkAllTokensUsed());
                        assertEquals(false, lt.hasMoreElements());
                    }
                } catch (MinimaParseException ex) {
                    fail();
                }
            }

            assertEquals(26, lt.getCurrentPosition());
            lt.goBackToken();
            assertEquals(25, lt.getCurrentPosition());
            lt.goBackToken();
            assertEquals(24, lt.getCurrentPosition());
            lt.goBackToken();
            assertEquals(23, lt.getCurrentPosition());
            lt.goBackToken();
            assertEquals(22, lt.getCurrentPosition());
            lt.goBackToken();
            assertEquals(21, lt.getCurrentPosition());
            lt.goBackToken();
            assertEquals(20, lt.getCurrentPosition());
        } catch (MinimaParseException ex) {
            fail();
        } 
    }

}
