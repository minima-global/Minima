package org.minima.utils.sphincs;

import java.util.ArrayList;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.kissvm.Contract;
import org.minima.kissvm.values.NumberValue;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.StateVariable;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.sphincs.FORS.FORSSignature;

public class TransactionTest {

	public static void log(String zMessage) {
		System.out.println(zMessage);
	}
	
	public static String getOutCoinString(Coin zCoin) {
		
		String keepstate=(""+zCoin.storeState()).toUpperCase();
		return zCoin.getAddress().to0xString()+"SPHINCS"+zCoin.getAmount().toString()+"SPHINCS"+zCoin.getTokenID().to0xString()+keepstate;
	}
	
	public static void main(String[] zArgs) {
	
		log("Start SPHINCS..");
		
		//Base seed for SPHINCS+
		MiniData seed 	 = new MiniData("0x0011223344"); 
		
		SPHINCS sphincs = new SPHINCS();
		sphincs.initSeed(seed);
		
		log("SPHINCS public key : "+sphincs.getPublicKey().to0xString());
		log("SPHINCS total WOTS keys : "+sphincs.getTotalWotsKeys());
		
		//The KISSVM SPHINCS script
		String sphincsscript = SPHINCS.KISSVM_SPHINCS_SCRIPT.replace("#USER_PUBLIC_KEY", sphincs.getPublicKey().to0xString());
		
		//Create  txn..
		Transaction transaction 	= new Transaction();
		Witness witness 			= new Witness();
		
		//Add some coin
		int inputcoinnum  = 7;
		int outputcoinnum = 2;
		
		ArrayList<Coin> allinputcoins = new ArrayList<>();
		for(int i=0;i<inputcoinnum;i++) {
			Coin in = new Coin(new MiniData("0x0"+i), new MiniData("0x00"), MiniNumber.ONE, Token.TOKENID_MINIMA, false);	
			allinputcoins.add(in);
			transaction.addInput(in);
			witness.getAllCoinProofs().add(new CoinProof(in, new MMRProof()));
		}
		
		ArrayList<Coin> alloutputcoins = new ArrayList<>();
		for(int i=0;i<outputcoinnum;i++) {
			Coin out = new Coin(new MiniData("0xF"+i), new MiniData("0xAA"), MiniNumber.ONE, Token.TOKENID_MINIMA, false);
			transaction.addOutput(out);
			alloutputcoins.add(out);
		}
		
		/**
		 * Sign a message
		 */
		int totin 		= allinputcoins.size();
		String instring = allinputcoins.get(0).getCoinID().to0xString();
		for(int i=1;i<totin;i++) {
			Coin cc 	= allinputcoins.get(i);
			instring	= instring+cc.getCoinID().to0xString();
		}
		
		//Outputs
		int totout = alloutputcoins.size();
		String outstring = "";
		for(int i=0;i<totout;i++) {
			Coin cc 	= alloutputcoins.get(i);
			outstring	= outstring+getOutCoinString(cc);
		}
		
		//Now create the string..
		String fullstring	= totin+"SPHINCS"+totout+"SPHINCS"+instring+"COINJOIN"+outstring;
		
		//Now the full message
		MiniData message	= new MiniData(new MiniString(fullstring).getData());
		log("Message : "+message.to0xString());
		
		
		
		SPHINCSSignature sig = sphincs.signMessage(message);
		
		/*
		 * Add some state vars
		 */
		
		//The Minima Sig
		MiniData minisig = MiniData.getMiniDataVersion(sig.getWOTSSignature()); 
		StateVariable svminimasig 	= new StateVariable(100, minisig.to0xString());
		transaction.addStateVariable(svminimasig);
		
		//The FORS root
		StateVariable svforsroot 	= new StateVariable(101, sig.getFORSRoot().getData().to0xString());
		transaction.addStateVariable(svforsroot);
		
		//HORST trees
		FORSSignature forssignature = sig.getFORSSignature();
		for(int i=0;i<16;i++) {
			
			//The base state pos
			int statepos = i*5;
			
			//The HORST root
			MMRData horstroot 		= forssignature.getHORSTRoots().get(i);
			StateVariable svhorstroot	= new StateVariable(statepos, horstroot.getData().to0xString());
			transaction.addStateVariable(svhorstroot);
			
			//The HORST root proof
			MMRProof horstproof			= forssignature.getHORSTTreeProofs().get(i);
			MiniData horstproofdata		= MiniData.getMiniDataVersion(horstproof);
			StateVariable svhorstproof	= new StateVariable(statepos+1, horstproofdata.to0xString());
			transaction.addStateVariable(svhorstproof);
			
			//The SIG value (private key preimage of public key)
			MiniData privkeyval = forssignature.getHORSTSignature().getSignatureValues().get(i);
			StateVariable svprivkeyval	= new StateVariable(statepos+2, privkeyval.to0xString());
			transaction.addStateVariable(svprivkeyval);
			
			//The Public key root proof
			MMRProof privkeyproof		= forssignature.getHORSTSignature().getPublicKeyTreeProofs().get(i);
			MiniData privkeyproofdata	= MiniData.getMiniDataVersion(privkeyproof);
			StateVariable svsigvalproof	= new StateVariable(statepos+3, privkeyproofdata.to0xString());
			transaction.addStateVariable(svsigvalproof);
		}
		
		/*
		 * NOW - execute the Contract
		 */
		Contract contract = new Contract(sphincsscript, new ArrayList<MiniData>(), witness, transaction, new ArrayList<StateVariable>(), false);
		
		contract.setGlobalVariable("@TOTIN", new NumberValue(transaction.getAllInputs().size()));
		contract.setGlobalVariable("@TOTOUT", new NumberValue(transaction.getAllOutputs().size()));
		
		contract.run();
		
		System.out.println("Valid Contract : "+contract.isSuccess());
		System.out.println("Operations     : "+contract.getNumberOfInstructions());
		if(!contract.isSuccess()) {
			System.out.println("Trace      : "+contract.getCompleteTraceLog());
		}
		
	}
}
