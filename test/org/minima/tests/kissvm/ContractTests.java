package org.minima.tests.kissvm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Hashtable;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.functions.txn.input.GETINADDR;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;

public class ContractTests {

    @Test
    public void testConstructors() {
        {
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), null);
            assertEquals(0, ctr.getGlobalVariables().size());
            assertTrue(ctr.isParseOK());
            assertFalse(ctr.isException());
            assertEquals("", ctr.getException());
            assertEquals(false, ctr.isTrace());
            //assertEquals(0, ctr.getCompleteTraceLog().length()); // trace log enabled, even if zTrace parameter is false
            assertEquals(0, ctr.getNumberOfInstructions());
            assertEquals("", ctr.getMiniScript());
            assertFalse(ctr.isSuccess());
            assertFalse(ctr.isSuccessSet());
            assertTrue(ctr.isMonotonic());
            assertEquals(0, ctr.getAllVariables().size());
        }

        {
            String Script = "RETURN TRUE";

            ArrayList<MiniData> Signatures = new ArrayList<MiniData>();
            String SignaturesStr = "";
            for (int i = 20; i <= 64; i = i + 4) {
                if (i == 44) { // skip bit length 352
                    continue;
                }
                MiniData SingleSig = MiniData.getRandomData(i);
                Signatures.add(SingleSig);
                if (!SignaturesStr.isEmpty()) {
                    SignaturesStr = SignaturesStr + "#";
                }
                SignaturesStr = SignaturesStr + SingleSig.toString();
            }

            ArrayList<StateVariable> PrevStates = new ArrayList<StateVariable>();
            for (int i = 0; i < 16; i++) {
                PrevStates.add(new StateVariable(4 * i + 0, new BooleanValue(true).toString()));
                PrevStates.add(new StateVariable(4 * i + 1, new HexValue("0x12345678").toString()));
                PrevStates.add(new StateVariable(4 * i + 2, new NumberValue(i).toString()));
                PrevStates.add(new StateVariable(4 * i + 3, new StringValue("[ Hello World " + Integer.toString(4 * i + 3)).toString() + " ]"));
            }

            Contract ctr = new Contract(Script, SignaturesStr, new Witness(), new Transaction(), PrevStates);

            assertTrue(ctr.isParseOK());
            assertFalse(ctr.isException());
            assertEquals("", ctr.getException());
            //assertEquals(0, ctr.getCompleteTraceLog().length()); // trace log enabled, even if zTrace parameter is false

            ctr.run();
            assertTrue(ctr.isSuccessSet());
            ctr.setRETURNValue(false);
            assertTrue(ctr.isSuccessSet());
        }

        {
            Contract ctr = new Contract("RETURN TRUE", "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);

            assertTrue(ctr.isParseOK());
            assertFalse(ctr.isException());
            assertEquals("", ctr.getException());
            assertTrue(ctr.getCompleteTraceLog().length() > 0);
        }

//        {
//            Contract ctr = new Contract("/* Comment */ RETURN TRUE", "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
//
//            assertTrue(ctr.isParseOK());
//            assertFalse(ctr.isException());
//            assertEquals("", ctr.getException());
//            assertTrue(ctr.getCompleteTraceLog().length() > 0);
//        }
//
//        {
//            Contract ctr = new Contract("RETURN TRUE /* Comment */", "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
//
//            assertTrue(ctr.isParseOK());
//            assertFalse(ctr.isException());
//            assertEquals("", ctr.getException());
//            assertTrue(ctr.getCompleteTraceLog().length() > 0);
//        }
//
//        {
//            Contract ctr = new Contract("/* Comment */ RETURN TRUE /* Comment */", "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
//
//            assertTrue(ctr.isParseOK());
//            assertFalse(ctr.isException());
//            assertEquals("", ctr.getException());
//            assertTrue(ctr.getCompleteTraceLog().length() > 0);
//        }
//
//        {
//            Contract ctr = new Contract("/*Comment*/RETURN TRUE/*Comment*/", "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
//
//            assertTrue(ctr.isParseOK());
//            assertFalse(ctr.isException());
//            assertEquals("", ctr.getException());
//            assertTrue(ctr.getCompleteTraceLog().length() > 0);
//        }

        {
            String Script = "lorem ipsum";

            ArrayList<MiniData> Signatures = new ArrayList<MiniData>();
            String SignaturesStr = "";
            for (int i = 20; i <= 64; i = i + 4) {
                if (i == 44) { // skip bit length 352
                    continue;
                }
                MiniData SingleSig = MiniData.getRandomData(i);
                Signatures.add(SingleSig);
                if (!SignaturesStr.isEmpty()) {
                    SignaturesStr = SignaturesStr + "#";
                }
                SignaturesStr = SignaturesStr + SingleSig.toString();
            }

            ArrayList<StateVariable> PrevStates = new ArrayList<StateVariable>();
            for (int i = 0; i < 16; i++) {
                PrevStates.add(new StateVariable(4 * i + 0, new BooleanValue(true).toString()));
                PrevStates.add(new StateVariable(4 * i + 1, new HexValue("0x12345678").toString()));
                PrevStates.add(new StateVariable(4 * i + 2, new NumberValue(i).toString()));
                PrevStates.add(new StateVariable(4 * i + 3, new StringValue("[ Hello World " + Integer.toString(4 * i + 3)).toString() + " ]"));
            }

            Contract ctr = new Contract(Script, SignaturesStr, new Witness(), new Transaction(), PrevStates, true);

            assertFalse(ctr.isParseOK());
            assertTrue(ctr.isException());
            assertEquals("org.minima.kissvm.exceptions.MinimaParseException: Invalid Token where there should be a COMMMAND - lorem", ctr.getException());
            assertTrue(ctr.getCompleteTraceLog().length() > 0);
        }
    }

    @Test
    public void testGettersAndSetters() {
        {
            Hashtable<String, Value> Globals = new Hashtable<>();
            Globals.put("@SCRIPT", new StringValue("0"));
            Globals.put("@BLKNUM", new NumberValue(1));
            Globals.put("@INBLKNUM", new NumberValue(2));
            Globals.put("@INPUT", new NumberValue(3));
            Globals.put("@ADDRESS", new HexValue("0x04"));
            Globals.put("@TOKENID", new HexValue("0x05"));
            Globals.put("@AMOUNT", new NumberValue(6));

            Contract ctr = new Contract("", "", new Witness(), new Transaction(), null);
            ctr.setAllGlobalVariables(Globals);

            Hashtable<String, Value> RetrievedGlobals = ctr.getGlobalVariables();
            assertEquals(7, RetrievedGlobals.size());
            assertEquals("0", ((StringValue) RetrievedGlobals.get("@SCRIPT")).toString());
            assertEquals("1", ((NumberValue) RetrievedGlobals.get("@BLKNUM")).toString());
            assertEquals("2", ((NumberValue) RetrievedGlobals.get("@INBLKNUM")).toString());
            assertEquals("3", ((NumberValue) RetrievedGlobals.get("@INPUT")).toString());
            assertEquals("0x04", ((HexValue) RetrievedGlobals.get("@ADDRESS")).toString());
            assertEquals("0x05", ((HexValue) RetrievedGlobals.get("@TOKENID")).toString());
            assertEquals("6", ((NumberValue) RetrievedGlobals.get("@AMOUNT")).toString());
        }

        {
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), null);

            BooleanValue bv = new BooleanValue(true);
            HexValue hv = new HexValue(new MiniData());
            NumberValue nv = new NumberValue(0);
            StringValue sv = new StringValue("[]");

            ctr.setVariable("BooleanValue", bv);
            ctr.setVariable("HEXValue", hv);
            ctr.setVariable("NumberValue", nv);
            ctr.setVariable("ScriptValue", sv);
            ctr.setVariable("Array,0,1,2,3", nv);

            assertEquals(5, ctr.getAllVariables().size());
        }

        {
            GETINADDR fn = new GETINADDR();
            MinimaFunction mf = fn.getNewFunction();
            mf.addParameter(new ConstantExpression(new NumberValue(0)));
            Contract ctr = new Contract("", "", new Witness(), new Transaction(), null);
            assertThrows(ExecutionException.class, () -> {
                ctr.getBoolParam(0, mf);
            });
        }
    }

    @Test
    public void testExecution() {
        {
            String Script = "lorem ipsum";

            ArrayList<MiniData> Signatures = new ArrayList<MiniData>();
            String SignaturesStr = "";
            for (int i = 20; i <= 64; i = i + 4) {
                if (i == 44) { // skip bit length 352
                    continue;
                }
                MiniData SingleSig = MiniData.getRandomData(i);
                Signatures.add(SingleSig);
                if (!SignaturesStr.isEmpty()) {
                    SignaturesStr = SignaturesStr + "#";
                }
                SignaturesStr = SignaturesStr + SingleSig.toString();
            }

            ArrayList<StateVariable> PrevStates = new ArrayList<StateVariable>();
            for (int i = 0; i < 16; i++) {
                PrevStates.add(new StateVariable(4 * i + 0, new BooleanValue(true).toString()));
                PrevStates.add(new StateVariable(4 * i + 1, new HexValue("0x12345678").toString()));
                PrevStates.add(new StateVariable(4 * i + 2, new NumberValue(i).toString()));
                PrevStates.add(new StateVariable(4 * i + 3, new StringValue("[ Hello World " + Integer.toString(4 * i + 3)).toString() + " ]"));
            }

            Contract ctr = new Contract(Script, SignaturesStr, new Witness(), new Transaction(), PrevStates, true);

            ctr.run();
            assertTrue(ctr.isException());
            assertEquals("org.minima.kissvm.exceptions.MinimaParseException: Invalid Token where there should be a COMMMAND - lorem", ctr.getException());
            assertTrue(ctr.getCompleteTraceLog().length() > 0);
        }

        {
            String Script = "";
            Contract ctr = new Contract(Script, "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
            for (int i = 0; i < ctr.MAX_INSTRUCTIONS; i++) {
                Script = Script + "LET a = " + i + " ";
            }

            {
                ctr.run();
                assertTrue(ctr.isParseOK());
                assertFalse(ctr.isException());
                assertEquals("", ctr.getException());
            }
        }

        {
            String Script = "";
            Contract ctr = new Contract(Script, "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
            for (int i = 0; i < ctr.MAX_INSTRUCTIONS+1; i++) {
                Script = Script + "LET a = " + i + " ";
            }
            ctr = new Contract(Script, "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
            {
            	MinimaLogger.log("RUN THIS.. ");
                ctr.run();
                assertTrue(ctr.isParseOK());
                assertTrue(ctr.isException());
                assertEquals("org.minima.kissvm.exceptions.ExecutionException: MAX instruction number reached! 1025", ctr.getException());
            }
        }

        {
            String Script = "";
            Contract ctr = new Contract(Script, "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
            for (int i = 0; i < ctr.MAX_INSTRUCTIONS+1; i++) {
                Script = Script + "LET a = " + i + " ";
            }
            ctr = new Contract(Script, "", new Witness(), new Transaction(), new ArrayList<StateVariable>(), true);
            
            {
                ctr.run();
                assertTrue(ctr.isParseOK());
                assertTrue(ctr.isException());
                assertEquals("org.minima.kissvm.exceptions.ExecutionException: MAX instruction number reached! 1025", ctr.getException());
            }
        }
    }
}
