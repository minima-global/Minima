package org.minima.tests.kissvm.tokens;

import org.minima.kissvm.tokens.QuotedString;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.MinimaParseException;
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

public class QuotedStringTests {

    @Test
    public void testConstructors() {
        QuotedString qs = new QuotedString("");
        assertEquals("", qs.getDeQuotedString());
        assertEquals(0, qs.getQuotesSize());
    }

    @Test
    public void testGettersAndSetters() {
        {
            QuotedString qs = new QuotedString("");
            assertEquals("", qs.getDeQuotedString());
            //assertEquals("", qs.getQuote(0)); // Does not check array size
            assertThrows(IndexOutOfBoundsException.class, () -> { // should throw this
                qs.getQuote(0);
            });
            assertEquals(0, qs.getQuotesSize());
        }

        {
            //Stack overflow
            //QuotedString qs = new QuotedString("[");
            //assertEquals("", qs.getDeQuotedString());
            //assertEquals("[ ]", qs.getQuote(0));
            //assertEquals(1, qs.getQuotesSize());
        }

        {
            //StringIndexOutOfBoundsException
            //QuotedString qs = new QuotedString("[ ] [");
            //assertEquals("", qs.getDeQuotedString());
            //assertEquals("[ ]", qs.getQuote(0));
            //assertEquals(1, qs.getQuotesSize());
        }

        {
            //Stack overflos
            //QuotedString qs = new QuotedString("[ [ ] [ ] [   jhfjh");
            //assertEquals("", qs.getDeQuotedString());
            //assertEquals("[ ]", qs.getQuote(0));
            //assertEquals(1, qs.getQuotesSize());
        }

        {
            QuotedString qs = new QuotedString("[ ]");
            assertEquals(":0", qs.getDeQuotedString());
            assertEquals("[ ]", qs.getQuote(0));
            assertEquals(1, qs.getQuotesSize());
        }

        {
            QuotedString qs = new QuotedString("[ abc ]");
            assertEquals(":0", qs.getDeQuotedString());
            assertEquals("[ abc ]", qs.getQuote(0));
            assertEquals(1, qs.getQuotesSize());
        }

        {
            QuotedString qs = new QuotedString("[ abc def ]");
            assertEquals(":0", qs.getDeQuotedString());
            assertEquals("[ abc def ]", qs.getQuote(0));
            assertEquals(1, qs.getQuotesSize());
        }

        {
            QuotedString qs = new QuotedString("[ [ ] ]");
            assertEquals(":0", qs.getDeQuotedString());
            assertEquals("[ [ ] ]", qs.getQuote(0));
            assertEquals(1, qs.getQuotesSize());
        }

        {
            QuotedString qs = new QuotedString("[ abc [ def ] ghi ]");
            assertEquals(":0", qs.getDeQuotedString());
            assertEquals("[ abc [ def ] ghi ]", qs.getQuote(0));
            assertEquals(1, qs.getQuotesSize());
        }

        {
            QuotedString qs = new QuotedString("[ abc ] [ def ] [ ghi ]");
            assertEquals(":0 :1 :2", qs.getDeQuotedString());
            assertEquals("[ abc ]", qs.getQuote(0));
            assertEquals("[ def ]", qs.getQuote(1));
            assertEquals("[ ghi ]", qs.getQuote(2));
            assertEquals(3, qs.getQuotesSize());
        }
    }
}
