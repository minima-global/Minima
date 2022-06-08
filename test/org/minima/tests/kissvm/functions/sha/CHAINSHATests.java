package org.minima.tests.kissvm.functions.sha;

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
import org.minima.kissvm.functions.sha.PROOF;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

//HEXValue CHAINSHA (HEXValue data HEXValue chain)
public class CHAINSHATests {

    @Test
    public void testConstructors() {
        PROOF fn = new PROOF();
        MinimaFunction mf = fn.getNewFunction();

        assertEquals("PROOF", mf.getName());
        assertEquals(0, mf.getParameterNum());

        try {
            mf = MinimaFunction.getFunction("PROOF");
            assertEquals("PROOF", mf.getName());
            assertEquals(0, mf.getParameterNum());
        } catch (MinimaParseException ex) {
            fail();
        }
    }

    @Test
    public void testValidParams() {
//        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());
//
//        CHAINSHA fn = new CHAINSHA();
//
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x05")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0xD81A30C1E58C555CB88D7E505C203CAA2DAAFB90", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x06")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0x8338FF93DDE8B5CBFFDE2AAE8EDB4A0697F967DF6E7B8A9F", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x07")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0xA408E4407FF54B9D1B0D9C4F9BEFCF14A66B6D600D390FB72519E551", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x08")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0x652D86DCB7EECF8BE8BF4F7FBA8CDC4F9B3DDDAEEBBD5AFDF530371DE63C0A99", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x09")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0xAECFA944A78D4E02B69E406BDCCF492B24C1EE721663385C6137C41B69D3A28323167F60", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x0A")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0x3F094EBA163EF6CCAD857DEE91C134FDF53A3C00329BAE7C45C4B472F4595801ADEBD211CCD5E4F9", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x0B")));
//            //try { // Test fails due to missing bitlength 352 in keccak and chainsha
//            //    Value res = mf.runFunction(ctr);
//            //    assertEquals(Value.VALUE_HEX, res.getValueType());
//            //    assertEquals("0x7CF0DE0388F60A95A3072E94ED9370050486FEA30C168B5DB590B947640E0BD8D5A9B75687134E7E54F7E1E7", ((HEXValue) res).toString());
//            //} catch (ExecutionException ex) {
//            //    fail();
//            //}
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x0C")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0xCB8C29605FEF8F89B65A5B36482999CA502892C32DDCADB654F1F1618E8B01227F16C133B344C3AD9170032BF84435BC", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x0D")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0x7D72A6EAB829A72517AE7E3FA0651DCC4AA54FEB3917B052B7E19B6F7F6DA219A2919C53A8AF3B859D114B7D53E7D12E3D704303", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x0E")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0xAC645B91A2041CF268A529DA0282CFBFB5EA5C71B71ABBD7D97E5BEE76926AEEB535B659A77AADC2C434FB1C9E932933F9983CB3E1DEE87B", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x0F")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0x1E278FC8A991B595582F7D82E10BC0ED1455A936F6B3364C14A0E4293CCAF9137DD8973B08A5983C20769D2FB99FA73F28515D77485D7531EC80026D", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }
//        {
//            MinimaFunction mf = fn.getNewFunction();
//            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
//            mf.addParameter(new ConstantExpression(new HexValue("0x10")));
//            try {
//                Value res = mf.runFunction(ctr);
//                assertEquals(Value.VALUE_HEX, res.getValueType());
//                assertEquals("0x31E5BE7BF3553D12496D81BEBBE7663E3006C2583B2DC205A3428E4DAAB055273ACEABA66841A47E05E6A76CF6FF27EC701952C5671FCAA00FC5E24A576C3479", ((HexValue) res).toString());
//            } catch (ExecutionException ex) {
//                fail();
//            }
//        }

    }

    @Test
    public void testInvalidParams() {
        Contract ctr = new Contract("", "", new Witness(), new Transaction(), new ArrayList<>());

        PROOF fn = new PROOF();

        // Invalid param count
        {
            MinimaFunction mf = fn.getNewFunction();
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param domain
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new HexValue("0x04")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new HexValue("0x11")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        // Invalid param types
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
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
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new HexValue("0x01234567")));
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
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }

        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new BooleanValue(true)));
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
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
        {
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            mf.addParameter(new ConstantExpression(new StringValue("Hello World")));
            assertThrows(ExecutionException.class, () -> {
                Value res = mf.runFunction(ctr);
            });
        }
    }
}
