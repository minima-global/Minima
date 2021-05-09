package org.minima.tests.kissvm.functions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.NumberValue;

public class MinimaFunctionTests {

    @Test
    public void testConstructors() {

        MinimaFunction mf;
        try {
            mf = MinimaFunction.getFunction("CONCAT");
            assertEquals("CONCAT", mf.getName());
        } catch (MinimaParseException ex) {
            fail();
        }

        assertThrows(MinimaParseException.class, () -> {
            MinimaFunction.getFunction("DUMMYFUNCTION");
        });

        DUMMYFUNCTION df = new DUMMYFUNCTION();
        mf = df.getNewFunction();

        assertEquals("DUMMYFUNCTION", mf.getName());
        assertEquals(0, mf.getParameterNum());
    }

    @Test
    public void testGettersAndSetters() {
        DUMMYFUNCTION df = new DUMMYFUNCTION();
        MinimaFunction mf = df.getNewFunction();

        assertEquals("DUMMYFUNCTION", mf.getName());
        assertEquals(0, mf.getParameterNum());

        ConstantExpression ce1 = new ConstantExpression(new BooleanValue(true));
        mf.addParameter(ce1);
        assertEquals(1, mf.getParameterNum());
        try {
            assertEquals(ce1, mf.getParameter(0));
        } catch (ExecutionException e) {
            fail();
        }

        ConstantExpression ce2 = new ConstantExpression(new NumberValue(1));
        mf.addParameter(ce2);
        try {
            assertEquals(ce2, mf.getParameter(1));
        } catch (ExecutionException e) {
            fail();
        }

        assertThrows(ExecutionException.class, () -> {
            mf.getParameter(2);
        });

        assertEquals(2, mf.getParameterNum());
    }
}
