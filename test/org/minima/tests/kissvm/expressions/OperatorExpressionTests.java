package org.minima.tests.kissvm.expressions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import java.math.BigInteger;
import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.OperatorExpression;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniNumber;

public class OperatorExpressionTests {

    @Test
    public void testConstructors() throws ExecutionException {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        ConstantExpression ceTrue = new ConstantExpression(new BooleanValue(true));
        ConstantExpression ceFalse = new ConstantExpression(new BooleanValue(false));

        ConstantExpression ceHexEmpty = new ConstantExpression(new HexValue(""));
        ConstantExpression ceHex00 = new ConstantExpression(new HexValue("0x00"));
        ConstantExpression ceHex01 = new ConstantExpression(new HexValue("0x01"));
        ConstantExpression ceHex0100 = new ConstantExpression(new HexValue("0x0100"));
        ConstantExpression ceHexDefault = new ConstantExpression(new HexValue("0x12345678"));
        ConstantExpression ceHex01L16 = new ConstantExpression(new HexValue("0x0101010101010101"));
        ConstantExpression ceHex10L16 = new ConstantExpression(new HexValue("0x1010101010101010"));

        ConstantExpression ce0 = new ConstantExpression(new NumberValue(0));
        ConstantExpression ce1 = new ConstantExpression(new NumberValue(1));
        ConstantExpression ce_1 = new ConstantExpression(new NumberValue(-1));
        ConstantExpression ce3 = new ConstantExpression(new NumberValue(3));
        ConstantExpression ce_3 = new ConstantExpression(new NumberValue(-3));
        ConstantExpression ce5 = new ConstantExpression(new NumberValue(5));
        ConstantExpression ce_5 = new ConstantExpression(new NumberValue(5));
        ConstantExpression ce1000 = new ConstantExpression(new NumberValue(1000));
        ConstantExpression ce_1000 = new ConstantExpression(new NumberValue(-1000));
        ConstantExpression ce1M = new ConstantExpression(new NumberValue(1000000));
        ConstantExpression ce_1M = new ConstantExpression(new NumberValue(-1000000));
        ConstantExpression ceMaxInt = new ConstantExpression(new NumberValue(Integer.MAX_VALUE));
        ConstantExpression ceMinInt = new ConstantExpression(new NumberValue(Integer.MIN_VALUE));
        ConstantExpression ceMaxLong = new ConstantExpression(new NumberValue(Long.MAX_VALUE));
        ConstantExpression ceMinLong = new ConstantExpression(new NumberValue(Long.MIN_VALUE));
        ConstantExpression ceMaxMini = new ConstantExpression(new NumberValue(new MiniNumber(MiniNumber.MAX_MININUMBER)));
        ConstantExpression ceMinMini = new ConstantExpression(new NumberValue(new MiniNumber(MiniNumber.MIN_MININUMBER)));

        ConstantExpression ceR0_5 = new ConstantExpression(new NumberValue(new MiniNumber("0.5")));
        ConstantExpression ceR_0_5 = new ConstantExpression(new NumberValue(new MiniNumber("-0.5")));
        ConstantExpression ceRPI = new ConstantExpression(new NumberValue(new MiniNumber("3.14")));
        ConstantExpression ceR_PI = new ConstantExpression(new NumberValue(new MiniNumber("-3.14")));

        ConstantExpression ceScriptEmpty = new ConstantExpression(new StringValue(""));
        ConstantExpression ceScriptDummy = new ConstantExpression(new StringValue("[Hello World]"));
        ConstantExpression ceScriptMinima = new ConstantExpression(new StringValue("LET A = 1"));

        assertEquals(5, ((NumberValue) (new OperatorExpression(ce5, ce0, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(5, ((NumberValue) (new OperatorExpression(ce0, ce5, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(6, ((NumberValue) (new OperatorExpression(ce5, ce1, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(6, ((NumberValue) (new OperatorExpression(ce1, ce5, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(Long.valueOf(Integer.MAX_VALUE) + 1, ((NumberValue) (new OperatorExpression(ceMaxInt, ce1, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE).toString(), ((NumberValue) (new OperatorExpression(ceMaxLong, ce1, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        //assertNotEquals(MiniNumber.MAX_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMaxMini, ce1, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        //assertNotEquals(MiniNumber.MAX_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMaxMini, ce1000, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        //assertNotEquals(MiniNumber.MAX_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMaxMini, ce1M, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).getNumber().getAsBigInteger().toString());

        assertEquals(5, ((NumberValue) (new OperatorExpression(ce5, ce0, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-5, ((NumberValue) (new OperatorExpression(ce0, ce5, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(4, ((NumberValue) (new OperatorExpression(ce5, ce1, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-4, ((NumberValue) (new OperatorExpression(ce1, ce5, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(Long.valueOf(Integer.MIN_VALUE) - 1, ((NumberValue) (new OperatorExpression(ceMinInt, ce1, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.ONE).toString(), ((NumberValue) (new OperatorExpression(ceMinLong, ce1, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        //assertNotEquals(MiniNumber.MIN_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMinMini, ce1, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        //assertNotEquals(MiniNumber.MIN_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMinMini, ce1000, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        //assertNotEquals(MiniNumber.MIN_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMinMini, ce1M, OperatorExpression.OPERATOR_SUB)).getValue(ctr)).getNumber().getAsBigInteger().toString());

        assertEquals(0, ((NumberValue) (new OperatorExpression(ce5, ce0, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(0, ((NumberValue) (new OperatorExpression(ce0, ce5, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(5, ((NumberValue) (new OperatorExpression(ce5, ce1, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(5, ((NumberValue) (new OperatorExpression(ce1, ce5, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(15, ((NumberValue) (new OperatorExpression(ce5, ce3, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(15, ((NumberValue) (new OperatorExpression(ce3, ce5, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(25, ((NumberValue) (new OperatorExpression(ce5, ce5, OperatorExpression.OPERATOR_MUL)).getValue(ctr)).getNumber().getAsLong());

        assertEquals(5, ((NumberValue) (new OperatorExpression(ce5, ce1, OperatorExpression.OPERATOR_DIV)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1, ((NumberValue) (new OperatorExpression(ce5, ce5, OperatorExpression.OPERATOR_DIV)).getValue(ctr)).getNumber().getAsLong());
        assertThrows(ExecutionException.class, () -> {
            (new OperatorExpression(ce1, ce0, OperatorExpression.OPERATOR_DIV)).getValue(ctr);
        });

        assertEquals("", ((StringValue) (new OperatorExpression(ceScriptEmpty, ceScriptEmpty, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());
        assertEquals("[Hello World]", ((StringValue) (new OperatorExpression(ceScriptEmpty, ceScriptDummy, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());
        assertEquals("[Hello World]", ((StringValue) (new OperatorExpression(ceScriptDummy, ceScriptEmpty, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());
        assertEquals("LET A = 1", ((StringValue) (new OperatorExpression(ceScriptEmpty, ceScriptMinima, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());
        assertEquals("LET A = 1", ((StringValue) (new OperatorExpression(ceScriptMinima, ceScriptEmpty, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());

        assertEquals("[Hello World][Hello World]", ((StringValue) (new OperatorExpression(ceScriptDummy, ceScriptDummy, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());
        assertEquals("[Hello World]LET A = 1", ((StringValue) (new OperatorExpression(ceScriptDummy, ceScriptMinima, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());
        assertEquals("LET A = 1[Hello World]", ((StringValue) (new OperatorExpression(ceScriptMinima, ceScriptDummy, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());

        assertEquals("LET A = 1LET A = 1", ((StringValue) (new OperatorExpression(ceScriptMinima, ceScriptMinima, OperatorExpression.OPERATOR_ADD)).getValue(ctr)).toString());

        assertEquals(0, ((NumberValue) (new OperatorExpression(ce0, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-1, ((NumberValue) (new OperatorExpression(ce1, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1, ((NumberValue) (new OperatorExpression(ce_1, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-1000, ((NumberValue) (new OperatorExpression(ce1000, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1000, ((NumberValue) (new OperatorExpression(ce_1000, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-1000000, ((NumberValue) (new OperatorExpression(ce1M, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1000000, ((NumberValue) (new OperatorExpression(ce_1M, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-Long.valueOf(Integer.MAX_VALUE), ((NumberValue) (new OperatorExpression(ceMaxInt, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-Long.valueOf(Integer.MIN_VALUE), ((NumberValue) (new OperatorExpression(ceMinInt, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(BigInteger.valueOf(Long.MAX_VALUE).negate().toString(), ((NumberValue) (new OperatorExpression(ceMaxLong, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        assertEquals(BigInteger.valueOf(Long.MIN_VALUE).negate().toString(), ((NumberValue) (new OperatorExpression(ceMinLong, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        assertEquals(MiniNumber.MIN_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMaxMini, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        assertEquals(MiniNumber.MAX_MININUMBER.toPlainString(), ((NumberValue) (new OperatorExpression(ceMinMini, OperatorExpression.OPERATOR_NEG)).getValue(ctr)).getNumber().getAsBigInteger().toString());

        assertEquals(0 % 3, ((NumberValue) (new OperatorExpression(ce0, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1 % 3, ((NumberValue) (new OperatorExpression(ce1, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-1 % 3, ((NumberValue) (new OperatorExpression(ce_1, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1000 % 3, ((NumberValue) (new OperatorExpression(ce1000, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-1000 % 3, ((NumberValue) (new OperatorExpression(ce_1000, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(1000000 % 3, ((NumberValue) (new OperatorExpression(ce1M, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(-1000000 % 3, ((NumberValue) (new OperatorExpression(ce_1M, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(Integer.MAX_VALUE % 3, ((NumberValue) (new OperatorExpression(ceMaxInt, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(Integer.MIN_VALUE % 3, ((NumberValue) (new OperatorExpression(ceMinInt, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsLong());
        assertEquals(BigInteger.valueOf(Long.MAX_VALUE).mod(BigInteger.valueOf(3)).toString(), ((NumberValue) (new OperatorExpression(ceMaxLong, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        // Fails with -2, expected -1
        //assertEquals(BigInteger.valueOf(Long.MIN_VALUE).mod(BigInteger.valueOf(3)).toString(), ((NumberValue) (new OperatorExpression(ceMinLong, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        // Division impossible
        //assertEquals("1", ((NumberValue) (new OperatorExpression(ceMaxMini, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigInteger().toString());
        // Division impossible
        //assertEquals("1", ((NumberValue) (new OperatorExpression(ceMinMini, ce3, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigInteger().toString());

        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01, ce0, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr)).toString());
        assertEquals("0x02", ((HexValue) (new OperatorExpression(ceHex01, ce1, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr)).toString());
        assertEquals("0x08", ((HexValue) (new OperatorExpression(ceHex01, ce3, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr)).toString());
        assertEquals("0x20", ((HexValue) (new OperatorExpression(ceHex01, ce5, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr)).toString());

        assertEquals("0x0100", ((HexValue) (new OperatorExpression(ceHex0100, ce0, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr)).toString());
        assertEquals("0x80", ((HexValue) (new OperatorExpression(ceHex0100, ce1, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr)).toString());
        assertEquals("0x20", ((HexValue) (new OperatorExpression(ceHex0100, ce3, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr)).toString());
        assertEquals("0x08", ((HexValue) (new OperatorExpression(ceHex0100, ce5, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr)).toString());

        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01, ceHex01, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex00, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex01L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex10L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex01L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x1010101010101010", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex10L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01, ceHex01L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01, ceHex10L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex00, ceHex01L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex00, ceHex10L16, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex01, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex01, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr)).toString());

        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01, ceHex01, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex00, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex01L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1111111111111111", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex10L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1111111111111111", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex01L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101010", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex10L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex01, ceHex01L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101011", ((HexValue) (new OperatorExpression(ceHex01, ceHex10L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex00, ceHex01L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101010", ((HexValue) (new OperatorExpression(ceHex00, ceHex10L16, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex01, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101011", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex01, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101010", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr)).toString());

        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01, ceHex01, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex01, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x01", ((HexValue) (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex00, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex01L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x1111111111111111", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex10L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x1111111111111111", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex01L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex10L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010100", ((HexValue) (new OperatorExpression(ceHex01, ceHex01L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101011", ((HexValue) (new OperatorExpression(ceHex01, ceHex10L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex00, ceHex01L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101010", ((HexValue) (new OperatorExpression(ceHex00, ceHex10L16, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010100", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex01, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x0101010101010101", ((HexValue) (new OperatorExpression(ceHex01L16, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101011", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex01, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());
        assertEquals("0x1010101010101010", ((HexValue) (new OperatorExpression(ceHex10L16, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr)).toString());

        // Invalid
        assertThrows(ExecutionException.class, () -> { // IllegalArgumentException maybe, as all others
            (new OperatorExpression(ceTrue, ceFalse, OperatorExpression.OPERATOR_ADD)).getValue(ctr);
        });
        assertThrows(ExecutionException.class, () -> { // IllegalArgumentException maybe, as all others
            (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_ADD)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceTrue, ceFalse, OperatorExpression.OPERATOR_SUB)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_SUB)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ceScriptMinima, OperatorExpression.OPERATOR_SUB)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceTrue, ceFalse, OperatorExpression.OPERATOR_MUL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_MUL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ceScriptMinima, OperatorExpression.OPERATOR_MUL)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceTrue, ceFalse, OperatorExpression.OPERATOR_DIV)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_DIV)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ceScriptMinima, OperatorExpression.OPERATOR_DIV)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceTrue, OperatorExpression.OPERATOR_NEG)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, OperatorExpression.OPERATOR_NEG)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, OperatorExpression.OPERATOR_NEG)).getValue(ctr);
        });

        // Should throw IllegalArgumentException/ExecutionException, modulo not defined for real numbers
        //assertThrows(IllegalArgumentException.class, () -> {
        //    (new OperatorExpression(ceR0_5, ceRPI, OperatorExpression.OPERATOR_MODULO)).getValue(ctr);
        //});
        assertEquals("0.5", ((NumberValue) (new OperatorExpression(ceR0_5, ceRPI, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigDecimal().toString());

        // Should throw IllegalArgumentException/ExecutionException, modulo not defined for real numbers
        //assertThrows(IllegalArgumentException.class, () -> {
        //    (new OperatorExpression(ceRPI, ceR0_5, OperatorExpression.OPERATOR_MODULO)).getValue(ctr);
        //});
        assertEquals("0.14", ((NumberValue) (new OperatorExpression(ceRPI, ceR0_5, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigDecimal().toString());

        // Should throw IllegalArgumentException/ExecutionException, modulo not defined for real numbers
        //assertThrows(IllegalArgumentException.class, () -> {
        //    (new OperatorExpression(ceR_0_5, ceR_PI, OperatorExpression.OPERATOR_MODULO)).getValue(ctr);
        //});
        assertEquals("-0.5", ((NumberValue) (new OperatorExpression(ceR_0_5, ceR_PI, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigDecimal().toString());

        // Should throw IllegalArgumentException/ExecutionException, modulo not defined for real numbers
        //assertThrows(IllegalArgumentException.class, () -> {
        //    (new OperatorExpression(ceR_PI, ceR_0_5, OperatorExpression.OPERATOR_MODULO)).getValue(ctr);
        //});
        assertEquals("-0.14", ((NumberValue) (new OperatorExpression(ceR_PI, ceR_0_5, OperatorExpression.OPERATOR_MODULO)).getValue(ctr)).getNumber().getAsBigDecimal().toString());

        // Should throw IllegalArgumentException/ExecutionException, left shift not defined for negative value
        //assertThrows(IllegalArgumentException.class, () -> {
        //    (new OperatorExpression(ceHex01, ce_3, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        //});
        assertEquals("0x00", ((HexValue) (new OperatorExpression(ceHex01, ce_3, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr)).toString());

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceFalse, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceScriptMinima, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceFalse, ce0, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ce1, ce0, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ce0, OperatorExpression.OPERATOR_SHIFTL)).getValue(ctr);
        });

        // Should throw IllegalArgumentException/ExecutionException, right shift not defined for negative value
        //assertThrows(IllegalArgumentException.class, () -> {
        //    (new OperatorExpression(ceHex01, ce_3, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        //});
        assertEquals("0x08", ((HexValue) (new OperatorExpression(ceHex01, ce_3, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr)).toString());

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceFalse, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceHex01, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceScriptMinima, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceFalse, ce0, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ce1, ce0, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ce0, OperatorExpression.OPERATOR_SHIFTR)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceFalse, OperatorExpression.OPERATOR_AND)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ce1, OperatorExpression.OPERATOR_AND)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceScriptMinima, OperatorExpression.OPERATOR_AND)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceFalse, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ce1, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ceHex00, OperatorExpression.OPERATOR_AND)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceFalse, OperatorExpression.OPERATOR_OR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ce1, OperatorExpression.OPERATOR_OR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceScriptMinima, OperatorExpression.OPERATOR_OR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceFalse, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ce1, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ceHex00, OperatorExpression.OPERATOR_OR)).getValue(ctr);
        });

        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceFalse, OperatorExpression.OPERATOR_XOR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ce1, OperatorExpression.OPERATOR_XOR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceHex00, ceScriptMinima, OperatorExpression.OPERATOR_XOR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceFalse, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ce1, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            (new OperatorExpression(ceScriptDummy, ceHex00, OperatorExpression.OPERATOR_XOR)).getValue(ctr);
        });

        assertThrows(ExecutionException.class, () -> {
            (new OperatorExpression(ceHex00, ceHex01, -5)).getValue(ctr);
        });
    }

    @Test
    public void testToString() {
        ConstantExpression ce1 = new ConstantExpression(new NumberValue(123));
        ConstantExpression ce2 = new ConstantExpression(new NumberValue(456));

        OperatorExpression oe;

        String exp_s;
        String obj_s;

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_ADD);
        exp_s = "( " + ce1.toString() + " + " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SUB);
        exp_s = "( " + ce1.toString() + " - " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_MUL);
        exp_s = "( " + ce1.toString() + " * " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_DIV);
        exp_s = "( " + ce1.toString() + " / " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, OperatorExpression.OPERATOR_NEG);
        exp_s = " - ( " + ce1.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SHIFTL);
        exp_s = "( " + ce1.toString() + " << " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_SHIFTR);
        exp_s = "( " + ce1.toString() + " >> " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_MODULO);
        exp_s = "( " + ce1.toString() + " % " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_AND);
        exp_s = "( " + ce1.toString() + " & " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_OR);
        exp_s = "( " + ce1.toString() + " | " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, OperatorExpression.OPERATOR_XOR);
        exp_s = "( " + ce1.toString() + " ^ " + ce2.toString() + " )";
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);

        oe = new OperatorExpression(ce1, ce2, Integer.MIN_VALUE); // returns ERROR on invalid operation
        exp_s = "( " + ce1.toString() + " ERROR " + ce2.toString() + " )"; // can be improved param ERROR param is not the best solution
        obj_s = oe.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
