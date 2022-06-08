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
import org.minima.kissvm.functions.txn.input.VERIFYIN;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;

//BooleanValue VERIFYIN (NumberValue input HEXValue address NumberValue amount HEXValue tokenind [NumberValue amountchecktype])
public class VERIFYINTests {

    @Test
    public void testConstructors() {
        VERIFYIN fn = new VERIFYIN();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("VERIFYIN", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("VERIFYIN");
            assertEquals("VERIFYIN", mf.getName());
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
        Address addr5 = newSimpleAddress();
        Address addr6 = newSimpleAddress();

        Witness w = new Witness();

        Token tmp = new Token(MiniData.getRandomData(16),
                MiniNumber.TEN,
                MiniNumber.MILLION,
                new MiniString("TestToken"),
                new MiniString("Hello from TestToken"));
        
        Transaction trx = new Transaction();

        Coin in1 = new Coin(MiniData.getRandomData(16), addr1.getAddressData(), new MiniNumber("50"), Token.TOKENID_MINIMA);
        trx.addInput(in1);

        Coin in2 = new Coin(MiniData.getRandomData(16), addr2.getAddressData(), new MiniNumber("75"), tmp.getTokenID());
        in2.setToken(tmp);
        trx.addInput(in2);

        Coin in3 = new Coin(MiniData.getRandomData(16), addr3.getAddressData(), new MiniNumber("1"), MiniData.getRandomData(16));
        trx.addInput(in3);

        Coin out1 = new Coin(MiniData.getRandomData(16), addr4.getAddressData(), new MiniNumber("40"), Token.TOKENID_MINIMA);
        trx.addOutput(out1);

        Coin out2 = new Coin(MiniData.getRandomData(16), addr5.getAddressData(), new MiniNumber("30"), tmp.getTokenID());
        out2.setToken(tmp);
        trx.addOutput(out2);

        Coin out3 = new Coin(MiniData.getRandomData(16), addr6.getAddressData(), new MiniNumber("1"), in3.getTokenID());
        out3.setToken(in3.getToken());
        trx.addOutput(out3);

        try {
            w.addScript(new ScriptProof(addr1.getScript()));
            w.addScript(new ScriptProof(addr2.getScript()));
        } catch (Exception ex) {
            fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        VERIFYIN fn = new VERIFYIN();

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue(addr1.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(in1.getAmount())));
            mf.addParameter(new ConstantExpression(new HexValue(in1.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isTrue());
            } catch (ExecutionException ex) {
                fail();
            }
        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new NumberValue(0)));
//            mf.addParameter(new ConstantExpression(new HexValue(addr1.getAddressData())));
//            mf.addParameter(new ConstantExpression(new NumberValue(in1.getAmount())));
//            mf.addParameter(new ConstantExpression(new HexValue(in1.getTokenID())));
//            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
//                assertEquals(true, ((BooleanValue) res).isTrue());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new NumberValue(0)));
//            mf.addParameter(new ConstantExpression(new HexValue(addr1.getAddressData())));
//            mf.addParameter(new ConstantExpression(new NumberValue(in1.getAmount())));
//            mf.addParameter(new ConstantExpression(new HexValue(in1.getTokenID())));
//            mf.addParameter(new ConstantExpression(new NumberValue(1)));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
//                assertEquals(true, ((BooleanValue) res).isTrue());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue(addr2.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(in1.getAmount())));
            mf.addParameter(new ConstantExpression(new HexValue(in1.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isFalse());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue(addr1.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount())));
            mf.addParameter(new ConstantExpression(new HexValue(in1.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isFalse());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue(addr1.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(in1.getAmount())));
            mf.addParameter(new ConstantExpression(new HexValue(MiniData.getRandomData(16))));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isFalse());
            } catch (ExecutionException ex) {
                fail();
            }
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new HexValue(addr2.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(tmp.getScaledTokenAmount(in2.getAmount()))));
//            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount().mult(tp.getScaleFactor()))));
            mf.addParameter(new ConstantExpression(new HexValue(in2.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isTrue());
            } catch (ExecutionException ex) {
                fail();
            }
        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new NumberValue(1)));
//            mf.addParameter(new ConstantExpression(new HexValue(addr2.getAddressData())));
//            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount().mult(tp.getScaleFactor()))));
//            mf.addParameter(new ConstantExpression(new HexValue(in2.getTokenID())));
//            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
//                assertEquals(true, ((BooleanValue) res).isTrue());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new NumberValue(1)));
//            mf.addParameter(new ConstantExpression(new HexValue(addr2.getAddressData())));
//            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount().mult(tp.getScaleFactor()))));
//            mf.addParameter(new ConstantExpression(new HexValue(in2.getTokenID())));
//            mf.addParameter(new ConstantExpression(new NumberValue(1)));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
//                assertEquals(true, ((BooleanValue) res).isTrue());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new HexValue(addr1.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(tmp.getScaledTokenAmount(in2.getAmount()))));
//            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount().mult(tp.getScaleFactor()))));
            mf.addParameter(new ConstantExpression(new HexValue(in2.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isFalse());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new HexValue(addr2.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount())));
            mf.addParameter(new ConstantExpression(new HexValue(in2.getTokenID())));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isFalse());
            } catch (ExecutionException ex) {
                fail();
            }
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new HexValue(addr2.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(tmp.getScaledTokenAmount(in2.getAmount()))));
//            mf.addParameter(new ConstantExpression(new NumberValue(in2.getAmount().mult(tp.getScaleFactor()))));
            mf.addParameter(new ConstantExpression(new HexValue(MiniData.getRandomData(16))));
            try {
                Value res = mf.runFunction(ctr);
                assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                assertEquals(true, ((BooleanValue) res).isFalse());
            } catch (ExecutionException ex) {
                fail();
            }
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(2)));
            mf.addParameter(new ConstantExpression(new HexValue(addr3.getAddressData())));
            mf.addParameter(new ConstantExpression(new NumberValue(tmp.getScaledTokenAmount(in2.getAmount()))));
//            mf.addParameter(new ConstantExpression(new NumberValue(in3.getAmount().mult(tp.getScaleFactor()))));
            mf.addParameter(new ConstantExpression(new HexValue(in3.getTokenID())));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }

    @Test
    public void testInvalidParams() {

    	Address addr1 = newSimpleAddress();
        Address addr2 = newSimpleAddress();
        Address addr3 = newSimpleAddress();
        Address addr4 = newSimpleAddress();
        Address addr5 = newSimpleAddress();
        Address addr6 = newSimpleAddress();

        Witness w = new Witness();

        Token tmp = new Token(MiniData.getRandomData(16),
                MiniNumber.TEN,
                MiniNumber.MILLION,
                new MiniString("TestToken"),
                new MiniString("Hello from TestToken"));
        
        Transaction trx = new Transaction();

        Coin in1 = new Coin(MiniData.getRandomData(16), addr1.getAddressData(), new MiniNumber("50"), Token.TOKENID_MINIMA);
        trx.addInput(in1);

        Coin in2 = new Coin(MiniData.getRandomData(16), addr2.getAddressData(), new MiniNumber("75"), tmp.getTokenID());
        in2.setToken(tmp);
        trx.addInput(in2);

        Coin in3 = new Coin(MiniData.getRandomData(16), addr3.getAddressData(), new MiniNumber("1"), MiniData.getRandomData(16));
        trx.addInput(in3);

        Coin out1 = new Coin(MiniData.getRandomData(16), addr4.getAddressData(), new MiniNumber("40"), Token.TOKENID_MINIMA);
        trx.addOutput(out1);

        Coin out2 = new Coin(MiniData.getRandomData(16), addr5.getAddressData(), new MiniNumber("30"), tmp.getTokenID());
        out2.setToken(tmp);
        trx.addOutput(out2);

        Coin out3 = new Coin(MiniData.getRandomData(16), addr6.getAddressData(), new MiniNumber("1"), in3.getTokenID());
        out3.setToken(in3.getToken());
        trx.addOutput(out3);

        try {
            w.addScript(new ScriptProof(addr1.getScript()));
            w.addScript(new ScriptProof(addr2.getScript()));
        } catch (Exception ex) {
            fail();
        }

        Contract ctr = new Contract("", "", w, trx, new ArrayList<>());

        VERIFYIN fn = new VERIFYIN();

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
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
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
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            assertThrows(ExecutionException.class, () -> { // should throw this
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(35)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x12345678")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
