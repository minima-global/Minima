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
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.sphincs.FORS.FORSSignature;

public class TransactionTest {

	public static void log(String zMessage) {
		System.out.println(zMessage);
	}
	
	public static void main(String[] zArgs) {
	
		MiniData seed 	 = new MiniData("0x0011223344"); 
		
		//Verify
		//boolean verify = SPHINCS.verifySignature(message, sig, sphincs.getPublicKey());
		//log("Verify : "+ verify);
		
		//The SPHINCS script
		String sphincsscript = "LET sphincspublickey=0xC05DC6D3B52BD12B182AF924F8ABC72CBDF64371E067BF4ECD018FC594915EA0 IF @TOTIN EQ 1 THEN LET message=GETINID(0) ELSEIF @TOTIN EQ 2 THEN LET message=CONCAT(GETINID(0) GETINID(1)) ELSEIF @TOTIN EQ 3 THEN LET message=CONCAT(GETINID(0) CONCAT(GETINID(1) GETINID(2))) ENDIF RETURN TRUE LET message=CONCAT(GETINID(0) GETINID(1)) LET hashedmessage=SHA3(message) LET forsrootdata=STATE(101) ASSERT CHECKSIG(sphincspublickey forsrootdata STATE(100)) LET counter=0 WHILE counter LT 1 DO LET statepos=counter*5 LET horstroot=STATE(statepos) ASSERT PROOF(horstroot counter forsrootdata 120 STATE(statepos+1)) LET keypos=counter*2 LET ref=NUMBER(SUBSET(keypos keypos+2 hashedmessage)) ASSERT PROOF(SHA3(STATE(statepos+2)) ref horstroot 2147450880 STATE(statepos+3)) LET counter=INC(counter) ENDWHILE RETURN TRUE";
		
		//Create  txn..
		Transaction transaction 	= new Transaction();
		Witness witness 			= new Witness();
		
		//Add some coin
		Coin in1 = new Coin(new MiniData("0x01"), new MiniData("0x00"), MiniNumber.ONE, Token.TOKENID_MINIMA, false);
		Coin in2 = new Coin(new MiniData("0x02"), new MiniData("0x00"), MiniNumber.ONE, Token.TOKENID_MINIMA, false);
		
		transaction.addInput(in1);
		transaction.addInput(in2);
		
		witness.getAllCoinProofs().add(new CoinProof(in1, new MMRProof()));
		witness.getAllCoinProofs().add(new CoinProof(in2, new MMRProof()));
		
		Coin out1 = new Coin(new MiniData("0xF1"), new MiniData("0x00"), MiniNumber.ONE, Token.TOKENID_MINIMA, false);
		Coin out2 = new Coin(new MiniData("0xF1"), new MiniData("0x00"), MiniNumber.TWO, Token.TOKENID_MINIMA, false);
		
		/**
		 * Sign a message
		 */
		//MiniData message = new MiniData("0x998877661");
		MiniData message = in1.getCoinID().concat(in2.getCoinID());
		
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
			log("HORST root : "+i+" "+horstroot.toString());
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
		Contract contract = new Contract(sphincsscript, new ArrayList<MiniData>(), witness, transaction, new ArrayList<StateVariable>(), true);
		
		contract.setGlobalVariable("@TOTIN", new NumberValue(transaction.getAllInputs().size()));
		
		contract.run();
		
		
	}
}
