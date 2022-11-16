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
import org.minima.kissvm.functions.txn.input.GETINADDR;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

//HEXValue GETINADDR (NumberValue input)
public class GETINADDRTests {

    @Test
    public void testConstructors() {
        GETINADDR fn = new GETINADDR();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("GETINADDR", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("GETINADDR");
            assertEquals("GETINADDR", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    public static Address newSimpleAddress() {
    	//Random public key
    	MiniData pubk = MiniData.getRandomData(32);
    	
    	//Create a simple address
    	String simpleaddress = new String("RETURN SIGNEDBY("+pubk.to0xString()+")"); 
    	
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
            fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        GETINADDR fn = new GETINADDR();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_HEX, res.getValueType());
                assertEquals(addr1.getAddressData(), ((HexValue) res).getMiniData());
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
                assertEquals(addr2.getAddressData(), ((HexValue) res).getMiniData());
            } catch (ExecutionException ex) {
                fail();
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
            fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        GETINADDR fn = new GETINADDR();

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
