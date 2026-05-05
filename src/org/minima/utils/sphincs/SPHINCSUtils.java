package org.minima.utils.sphincs;

import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMRProof;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.utils.sphincs.FORS.FORSSignature;

public class SPHINCSUtils {

	/**
	 * The SPHINCS KISSVM script
	 * 
	 * Replace #USER_PUBLIC_KEY with the users public key
	 */
	public static final String KISSVM_SPHINCS_SCRIPT = "LET sphincspublickey=#USER_PUBLIC_KEY LET incoins=STRING(GETINID(0)) LET counter=1 WHILE counter LT @TOTIN DO LET incoins=incoins+STRING(GETINID(counter)) LET counter=INC(counter) ENDWHILE LET calcoutput=[LET returnvalue=STRING(GETOUTADDR($1))+[SPHINCS]+STRING(GETOUTAMT($1))+[SPHINCS]+STRING(GETOUTTOK($1))+STRING(GETOUTKEEPSTATE($1))] LET outcoins=FUNCTION(calcoutput 0) LET counter=1 WHILE counter LT @TOTOUT DO LET outcoins=outcoins+FUNCTION(calcoutput counter) LET counter=INC(counter) ENDWHILE LET hashedmessage=SHA3(STRING(@TOTIN)+[SPHINCS]+STRING(@TOTOUT)+[SPHINCS]+incoins+[COINJOIN]+outcoins) LET forsrootdata=STATE(101) ASSERT CHECKSIG(sphincspublickey forsrootdata STATE(100)) LET counter=0 WHILE counter LT 16 DO LET statepos=counter*5 LET horstroot=STATE(statepos) ASSERT PROOF(horstroot counter forsrootdata 120 STATE(statepos+1)) LET keypos=counter*2 LET ref=NUMBER(SUBSET(keypos keypos+2 hashedmessage)) ASSERT PROOF(SHA3(STATE(statepos+2)) ref horstroot 2147450880 STATE(statepos+3)) LET counter=INC(counter) ENDWHILE RETURN TRUE";
	
	
	public static String getKISSVMScript(MiniData zPublicKey) {
		return KISSVM_SPHINCS_SCRIPT.replace("#USER_PUBLIC_KEY", zPublicKey.to0xString());
	}
	
	/**
	 * Convert the output coin into a String representation 
	 */
	private static String getOutCoinString(Coin zCoin) {
		String keepstate=(""+zCoin.storeState()).toUpperCase();
		return zCoin.getAddress().to0xString()+"SPHINCS"+zCoin.getTokenAmount().toString()
				+"SPHINCS"+zCoin.getTokenID().to0xString()+keepstate;
	}
	
	/**
	 * Calculate the TransactionID based on all the transaction coins
	 */
	public static MiniData calculateTransactionID(Transaction zTransaction) {
		
		ArrayList<Coin> inputcoins 	= zTransaction.getAllInputs();
		ArrayList<Coin> outputcoins = zTransaction.getAllOutputs();
		
		int totin = inputcoins.size();
		String instring = inputcoins.get(0).getCoinID().to0xString();
		for(int i=1;i<totin;i++) {
			Coin cc 	= inputcoins.get(i);
			instring	= instring+cc.getCoinID().to0xString();
		}

		int totout = outputcoins.size();
		String outstring = "";
		for(int i=0;i<totout;i++) {
			Coin cc 	= outputcoins.get(i);
			outstring	= outstring+getOutCoinString(cc);
		}
		
		//Now create the complete message string..
		String fullstring	= totin+"SPHINCS"+totout+"SPHINCS"+instring+"COINJOIN"+outstring;
		
		//Now the full message
		MiniData message	= new MiniData(new MiniString(fullstring).getData());
		
		return message;
	}
	
	/**
	 * Set the transaction state variables..
	 */
	public static void setupTransaction(Transaction zTransaction, SPHINCSSignature zSignature) {
		/**
		 * Add the state Vars
		 */
		//The Minima Sig
		MiniData minisig = MiniData.getMiniDataVersion(zSignature.getWOTSSignature()); 
		StateVariable svminimasig 	= new StateVariable(100, minisig.to0xString());
		zTransaction.addStateVariable(svminimasig);
		
		//The FORS root
		StateVariable svforsroot 	= new StateVariable(101, zSignature.getFORSRoot().getData().to0xString());
		zTransaction.addStateVariable(svforsroot);
		
		//HORST trees
		FORSSignature forssignature = zSignature.getFORSSignature();
		for(int i=0;i<16;i++) {
			
			//The base state pos
			int statepos = i*5;
			
			//The HORST root
			MMRData horstroot 		= forssignature.getHORSTRoots().get(i);
			StateVariable svhorstroot	= new StateVariable(statepos, horstroot.getData().to0xString());
			zTransaction.addStateVariable(svhorstroot);
			
			//The HORST root proof
			MMRProof horstproof			= forssignature.getHORSTTreeProofs().get(i);
			MiniData horstproofdata		= MiniData.getMiniDataVersion(horstproof);
			StateVariable svhorstproof	= new StateVariable(statepos+1, horstproofdata.to0xString());
			zTransaction.addStateVariable(svhorstproof);
			
			//The SIG value (private key preimage of public key)
			MiniData privkeyval = forssignature.getHORSTSignature().getSignatureValues().get(i);
			StateVariable svprivkeyval	= new StateVariable(statepos+2, privkeyval.to0xString());
			zTransaction.addStateVariable(svprivkeyval);
			
			//The Public key root proof
			MMRProof privkeyproof		= forssignature.getHORSTSignature().getPublicKeyTreeProofs().get(i);
			MiniData privkeyproofdata	= MiniData.getMiniDataVersion(privkeyproof);
			StateVariable svsigvalproof	= new StateVariable(statepos+3, privkeyproofdata.to0xString());
			zTransaction.addStateVariable(svsigvalproof);
		}
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(zTransaction);
				
		//Calculate transid
		zTransaction.calculateTransactionID();
	}
}
