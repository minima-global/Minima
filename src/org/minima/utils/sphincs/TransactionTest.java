package org.minima.utils.sphincs;

import java.util.ArrayList;

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
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMRProof;
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
	
		//Base seed for SPHINCS+
		MiniData seed 	 = new MiniData("0x0011223344"); 
		
		//The KISSVM SPHINCS script
		String sphincsscript = "LET sphincspublickey=0xC05DC6D3B52BD12B182AF924F8ABC72CBDF64371E067BF4ECD018FC594915EA0 LET incoins=GETINID(0) LET counter=1 WHILE counter LT @TOTIN DO LET incoins=CONCAT(incoins GETINID(counter)) LET counter=INC(counter) ENDWHILE LET incoins=STRING(incoins) LET calcoutput=[LET returnvalue=STRING(GETOUTADDR($1))+[SPHINCS]+STRING(GETOUTAMT($1))+[SPHINCS]+STRING(GETOUTTOK($1))+STRING(GETOUTKEEPSTATE($1))] LET outcoins=FUNCTION(calcoutput 0) LET counter=1 WHILE counter LT @TOTOUT DO LET outcoins=outcoins+FUNCTION(calcoutput counter) LET counter=INC(counter) ENDWHILE LET hashedmessage=SHA3(incoins+[COINJOIN]+outcoins) LET forsrootdata=STATE(101) ASSERT CHECKSIG(sphincspublickey forsrootdata STATE(100)) LET counter=0 WHILE counter LT 16 DO LET statepos=counter*5 LET horstroot=STATE(statepos) ASSERT PROOF(horstroot counter forsrootdata 120 STATE(statepos+1)) LET keypos=counter*2 LET ref=NUMBER(SUBSET(keypos keypos+2 hashedmessage)) ASSERT PROOF(SHA3(STATE(statepos+2)) ref horstroot 2147450880 STATE(statepos+3)) LET counter=INC(counter) ENDWHILE RETURN TRUE";
		
		//Create  txn..
		Transaction transaction 	= new Transaction();
		Witness witness 			= new Witness();
		
		//Add some coin
		int inputcoinnum  = 8;
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
		MiniData inmessage = allinputcoins.get(0).getCoinID();
		for(int i=1;i<allinputcoins.size();i++) {
			Coin cc 	= allinputcoins.get(i);
			inmessage	= inmessage.concat(cc.getCoinID());
		}
		String instring	= inmessage.to0xString();
		
		//Outputs
		String outstring = "";
		for(int i=0;i<alloutputcoins.size();i++) {
			Coin cc 	= alloutputcoins.get(i);
			outstring	= outstring+getOutCoinString(cc);
		}
		
		//Now create the string..
		String fullstring	= instring+"COINJOIN"+outstring;
		log("Message IN String : "+instring);
		log("Message OUT String : "+outstring);
		
		//Now the full message
		MiniData message	= new MiniData(new MiniString(fullstring).getData());
		
		log("Message : "+message.to0xString());
		
		log("Start SPHINCS..");
		
		SPHINCS sphincs = new SPHINCS(seed);
		
		log("SPHINCS public key : "+sphincs.getPublicKey().to0xString());
		log("SPHINCS total WOTS keys : "+sphincs.getTotalWotsKeys());
		
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
			MiniData privkeyval = forssignature.getSignatureValues().get(i);
			StateVariable svprivkeyval	= new StateVariable(statepos+2, privkeyval.to0xString());
			transaction.addStateVariable(svprivkeyval);
			
			//The Public key root proof
			MMRProof privkeyproof		= forssignature.getPublicKeyTreeProofs().get(i);
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
