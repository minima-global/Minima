package org.minima.tests.kissvm.functions.sigs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.sigs.MULTISIG;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;

//BooleanValue MULTISIG (NumberValue required HEXValue sig1 … HEXValue sigN)
public class MULTISIGTests {

    @Test
    public void testConstructors() {
        MULTISIG fn = new MULTISIG();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("MULTISIG", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("MULTISIG");
            assertEquals("MULTISIG", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {

        ArrayList<MiniData> Signatures = new ArrayList<MiniData>();
//        String SignaturesStr = "";
        for (int i = 20; i <= 64; i = i + 4) {
            if (i == 44) { // skip bit length 352
                continue;
            }
            // MULTISIG is actually string search in a list of signatures
            // So we can simplify test without actual keys
            //MiniData Seed = MiniData.getRandomData(i);
            //MultiKey MKeySign = new MultiKey(Seed, new MiniNumber("16"), new MiniNumber("2"));
            //MiniData Data = MiniData.getRandomData(i);
            //MiniData SingleSig = MKeySign.sign(Data);
            //Signatures.add(SingleSig);

            MiniData SingleSig = MiniData.getRandomData(i);
            Signatures.add(SingleSig);

//            if (!SignaturesStr.isEmpty()) {
//                SignaturesStr = SignaturesStr + "#";
//            }
//            SignaturesStr = SignaturesStr + SingleSig.toString();
        }
        // MULTISIG is actually string search in a list of signatures
        // So we can simplify test without actual keys
        //MiniData UntrackedSeed = MiniData.getRandomData(20);
        //MultiKey UntrackedMKeySign = new MultiKey(UntrackedSeed, new MiniNumber("16"), new MiniNumber("2"));
        //MiniData UntrackedData = MiniData.getRandomData(20);
        //MiniData UntrackedSingleSig = UntrackedMKeySign.sign(UntrackedData);

        MiniData UntrackedSingleSig = MiniData.getRandomData(20);

        Contract ctr = new Contract("", Signatures, new Witness(), new Transaction(), new ArrayList<>());

        MULTISIG fn = new MULTISIG();

        for (int i = 1; i <= Signatures.size(); i++) {
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                for (int j = 0; j < i; j++) {
                    mf.addParameter(new ConstantExpression(new HexValue(Signatures.get(j))));
                }
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                    assertEquals("TRUE", ((BooleanValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(i)));
                for (int j = 0; j < i - 1; j++) {
                    mf.addParameter(new ConstantExpression(new HexValue(Signatures.get(j))));
                }
                mf.addParameter(new ConstantExpression(new HexValue(UntrackedSingleSig)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                    assertEquals("FALSE", ((BooleanValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
            {
                MinimaFunction mf = fn.getNewFunction();
                mf.addParameter(new ConstantExpression(new NumberValue(i + 1)));
                for (int j = 0; j < i; j++) {
                    mf.addParameter(new ConstantExpression(new HexValue(Signatures.get(j))));
                }
                mf.addParameter(new ConstantExpression(new HexValue(UntrackedSingleSig)));
                try {
                    Value res = mf.runFunction(ctr);
                    assertEquals(Value.VALUE_BOOLEAN, res.getValueType());
                    assertEquals("FALSE", ((BooleanValue) res).toString());
                } catch (ExecutionException ex) {
                    fail();
                }
            }
        }
    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        MULTISIG fn = new MULTISIG();

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

        // Invalid param domain
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(-1)));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> { // Should throw this, for negative value of valid sigs
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> { // Should throw this, for zero valid sigs
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param types
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(1)));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

    }
}
