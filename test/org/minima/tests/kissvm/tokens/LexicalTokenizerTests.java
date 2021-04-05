package org.minima.tests.kissvm.tokens;

import org.minima.kissvm.tokens.LexicalTokenizer;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.exceptions.SyntaxException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.tokens.Token;
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

public class LexicalTokenizerTests {

    @Test
    public void testConstructors() {
        LexicalTokenizer lt = new LexicalTokenizer(new ArrayList<Token>());
        assertThrows(MinimaParseException.class, () -> {
            lt.getNextToken();
        });
        assertEquals(0, lt.getCurrentPosition());
        assertEquals(true, lt.checkAllTokensUsed());
        assertEquals(false, lt.hasMoreElements());

        lt.goBackToken(); // Can create negative position value
        //assertEquals(0, lt.getCurrentPosition());
        assertEquals(-1, lt.getCurrentPosition());
    }

    @Test
    public void testGetters() {
        String Script = "a b c d e f g h i j k l m n o p q r s t u v w x y z";
        try {
            List<Token> tokens = Token.tokenize(Script);
            LexicalTokenizer lt = new LexicalTokenizer(tokens);

            for (int i = 0; i < tokens.size(); i++) {
                assertEquals(i, lt.getCurrentPosition());
                assertEquals(false, lt.checkAllTokensUsed());
                assertEquals(true, lt.hasMoreElements());

                Token t;
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
        } catch (SyntaxException ex) {
            fail();
        } 
    }

}
