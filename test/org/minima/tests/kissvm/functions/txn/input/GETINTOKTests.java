package org.minima.tests.kissvm.functions.txn.input;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.txn.input.GETINTOK;
import org.minima.kissvm.values.*;
import org.minima.objects.*;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

//HEXValue GETINTOK (NumberValue input)
public class GETINTOKTests {

    @Test
    public void testConstructors() {
        GETINTOK fn = new GETINTOK();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("GETINTOK", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("GETINTOK");
            assertEquals("GETINTOK", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            Assertions.fail();
        }
    }

    public static Address newSimpleAddress() {
        //Random public key
        MiniData pubk = MiniData.getRandomData(32);

        //Create a simple address
        String simpleaddress = new String("RETURN SIGNEDBY(" + pubk.to0xString() + ")");

        //Now create the address
        Address addr = new Address(simpleaddress);

        return addr;
    }

    @Test
    public void testValidParams() {

        Address addr1 = newSimpleAddress();
        Address addr2 = newSimpleAddress();
        Address addr3 = newSimpleAddress();
        Address addr4 = newSimpleAddress();

        Transaction trx = new Transaction();

        Coin in1 = new Coin(Coin.COINID_OUTPUT, addr1.getAddressData(), new MiniNumber("25"), Token.TOKENID_MINIMA);
        trx.addInput(in1);

        Coin in2 = new Coin(Coin.COINID_OUTPUT, addr2.getAddressData(), new MiniNumber("75"), Token.TOKENID_MINIMA);
        trx.addInput(in2);

        Coin out1 = new Coin(Coin.COINID_OUTPUT, addr3.getAddressData(), new MiniNumber("40"), Token.TOKENID_MINIMA);
        trx.addOutput(out1);

        Coin out2 = new Coin(Coin.COINID_OUTPUT, addr4.getAddressData(), new MiniNumber("60"), Token.TOKENID_MINIMA);
        trx.addOutput(out2);

        Witness w = new Witness();
        try {
            w.addScript(new ScriptProof(addr1.getScript()));
            w.addScript(new ScriptProof(addr2.getScript()));
        } catch (Exception ex) {
            Assertions.fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        GETINTOK fn = new GETINTOK();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals(in1.getTokenID(), ((HexValue) res).getMiniData());
            } catch (ExecutionException ex) {
                Assertions.fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals(in2.getTokenID(), ((HexValue) res).getMiniData());
            } catch (ExecutionException ex) {
                Assertions.fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {

        Address addr1 = newSimpleAddress();
        Address addr2 = newSimpleAddress();
        Address addr3 = newSimpleAddress();
        Address addr4 = newSimpleAddress();

        Transaction trx = new Transaction();

        Coin in1 = new Coin(Coin.COINID_OUTPUT, addr1.getAddressData(), new MiniNumber("25"), Token.TOKENID_MINIMA);
        trx.addInput(in1);

        Coin in2 = new Coin(Coin.COINID_OUTPUT, addr2.getAddressData(), new MiniNumber("75"), Token.TOKENID_MINIMA);
        trx.addInput(in2);

        Coin out1 = new Coin(Coin.COINID_OUTPUT, addr3.getAddressData(), new MiniNumber("40"), Token.TOKENID_MINIMA);
        trx.addOutput(out1);

        Coin out2 = new Coin(Coin.COINID_OUTPUT, addr4.getAddressData(), new MiniNumber("60"), Token.TOKENID_MINIMA);
        trx.addOutput(out2);

        Witness w = new Witness();
        try {
            w.addScript(new ScriptProof(addr1.getScript()));
            w.addScript(new ScriptProof(addr2.getScript()));
        } catch (Exception ex) {
            Assertions.fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        GETINTOK fn = new GETINTOK();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param domain
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
            assertThrows(ExecutionException.class, () -> { // should throw this
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(35)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param types
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
