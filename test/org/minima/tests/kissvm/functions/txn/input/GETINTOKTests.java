package org.minima.tests.kissvm.functions.txn.input;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.database.MinimaDB;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.txn.input.GETINTOK;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniNumber;

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
            fail();
        }
    }

    @Test
    public void testValidParams() {

        MinimaDB mdb = new MinimaDB();

        Address addr1 = mdb.getUserDB().newSimpleAddress();
        Address addr2 = mdb.getUserDB().newSimpleAddress();
        Address addr3 = mdb.getUserDB().newSimpleAddress();
        Address addr4 = mdb.getUserDB().newSimpleAddress();

        Transaction trx = new Transaction();

        Coin in1 = new Coin(Coin.MINIMA_TOKENID, addr1.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        trx.addInput(in1);

        Coin in2 = new Coin(Coin.MINIMA_TOKENID, addr2.getAddressData(), new MiniNumber("75"), Coin.MINIMA_TOKENID);
        trx.addInput(in2);

        Coin out1 = new Coin(Coin.MINIMA_TOKENID, addr3.getAddressData(), new MiniNumber("40"), Coin.MINIMA_TOKENID);
        trx.addOutput(out1);

        Coin out2 = new Coin(Coin.MINIMA_TOKENID, addr4.getAddressData(), new MiniNumber("60"), Coin.MINIMA_TOKENID);
        trx.addOutput(out2);

        Witness w = new Witness();
        try {
            w.addScript(addr1.getScript(), in1.getAddress().getLength() * 8);
            w.addScript(addr2.getScript(), in2.getAddress().getLength() * 8);
        } catch (Exception ex) {
            fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        GETINTOK fn = new GETINTOK();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals(in1.getTokenID(), ((HEXValue) res).getMiniData());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals(in2.getTokenID(), ((HEXValue) res).getMiniData());
            } catch (ExecutionException ex) {
                fail();
            }
        }
    }

    @Test
    public void testInvalidParams() {

        MinimaDB mdb = new MinimaDB();

        Address addr1 = mdb.getUserDB().newSimpleAddress();
        Address addr2 = mdb.getUserDB().newSimpleAddress();
        Address addr3 = mdb.getUserDB().newSimpleAddress();
        Address addr4 = mdb.getUserDB().newSimpleAddress();

        Transaction trx = new Transaction();

        Coin in1 = new Coin(Coin.MINIMA_TOKENID, addr1.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        trx.addInput(in1);

        Coin in2 = new Coin(Coin.MINIMA_TOKENID, addr2.getAddressData(), new MiniNumber("75"), Coin.MINIMA_TOKENID);
        trx.addInput(in2);

        Coin out1 = new Coin(Coin.MINIMA_TOKENID, addr3.getAddressData(), new MiniNumber("40"), Coin.MINIMA_TOKENID);
        trx.addOutput(out1);

        Coin out2 = new Coin(Coin.MINIMA_TOKENID, addr4.getAddressData(), new MiniNumber("60"), Coin.MINIMA_TOKENID);
        trx.addOutput(out2);

        Witness w = new Witness();
        try {
            w.addScript(addr1.getScript(), in1.getAddress().getLength() * 8);
            w.addScript(addr2.getScript(), in2.getAddress().getLength() * 8);
        } catch (Exception ex) {
            fail();
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
            mf.addParameter(new ConstantExpression(new HEXValue("0x12345678")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new ScriptValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
