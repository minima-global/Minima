package org.minima.system.brains;

import java.util.ArrayList;
import java.util.HashSet;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.kissvm.Contract;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.utils.MinimaLogger;

public class TxPoWChecker {

	/**
	 * Parallel check all the transactions in this block
	 */
	public static boolean checkTxPoWBlock(TxPoWTreeNode zParentNode, TxPoW zTxPoW, ArrayList<TxPoW> zTransactions) {
		
		try {
			//Check the time of the block is greater than the media time
			MiniNumber mediantime = TxPoWGenerator.getMedianTime(zParentNode);
			if(zTxPoW.getTimeMilli().isLess(mediantime)) {
				MinimaLogger.log("Invalid TxPoW block with millitime less than median "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check all the input coinid are Unique - use the MMR proofs! CoinID could be Eltoo
			ArrayList<String> allcoinid = new ArrayList<>();
			if(zTxPoW.isTransaction()) {
				ArrayList<CoinProof> proofs = zTxPoW.getWitness().getAllCoinProofs();
				for(CoinProof proof : proofs) {
					allcoinid.add(proof.getCoin().getCoinID().to0xString());
				}
			}
			for(TxPoW txpow : zTransactions) {
				if(txpow.isTransaction()) {
					ArrayList<CoinProof> proofs = txpow.getWitness().getAllCoinProofs();
					for(CoinProof proof : proofs) {
						allcoinid.add(proof.getCoin().getCoinID().to0xString());
					}
				}
			}
			
			//Convert to unique Set and check equal size
			HashSet<String> coinset = new HashSet<>(allcoinid);
			if(coinset.size() != allcoinid.size()) {
				MinimaLogger.log("Invalid TxPoW Block with non unique CoinID "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Get the Parent MMR
			MMR parentMMR = zParentNode.getMMR();
			
			//First check this
			if(zTxPoW.isTransaction()) {
				boolean valid = checkTxPoW(parentMMR, zTxPoW, zTxPoW.getBlockNumber());
				if(!valid) {
					return false;
				}
			}
			
			//Now check all the internal Transactions
			for(TxPoW txpow : zTransactions) {
				boolean valid = checkTxPoW(parentMMR, txpow, zTxPoW.getBlockNumber());
				if(!valid) {
					return false;
				}
			}
			
			//Construct the MMR.. to see if it is correct..
			TxBlock txblock 		= new TxBlock(parentMMR, zTxPoW, zTransactions);
			TxPoWTreeNode node 		= new TxPoWTreeNode(txblock, false);

			//Check Correct..
			MMRData root = node.getMMR().getRoot();
			if(!root.getData().isEqual(zTxPoW.getMMRRoot()) || !root.getValue().isEqual(zTxPoW.getMMRTotal())) {
				MinimaLogger.log("ERROR : MMR in TxPOW block and calculated don't match! @ "+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPoWID());
				return false;
			}
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR checking TxPoW Block..");
			MinimaLogger.log(exc);
			
			return false;
		}
		
		return true;
	}
	
	public static boolean checkTxPoW(MMR zTipMMR, TxPoW zTxPoW, MiniNumber zBlock) throws Exception {
		//Get the main Transaction..
		Transaction transaction = zTxPoW.getTransaction();
		if(transaction.isEmpty()) {
			return true;
		}
		
		//Basic tests and Check Values add up..
		if(!transaction.checkValid()) {
			MinimaLogger.log("Invalid Transaction Inputs and Outputs.. "+transaction.toJSON());
			return false;
		}
		
		//Get the Witness
		Witness witness = zTxPoW.getWitness();
		
		//Check the Inputs Coins..
		ArrayList<Coin> inputs 			= transaction.getAllInputs();
		int ins = inputs.size();
		
		ArrayList<CoinProof> mmrproofs 	= witness.getAllCoinProofs();
		
		//Check we have the correct amount..
		if(ins != mmrproofs.size()) {
			MinimaLogger.log("MISSING MMR Proofs Inputs:"+ins+" MMRProofs:"+mmrproofs.size()+" @ "+zTxPoW.getTxPoWID());
			return false;
		}
		
		//List of CoinID used..
		ArrayList<String> allcoinsused = new ArrayList<>();
		
		//Cycle through and check..
		for(int i=0;i<ins;i++) {
			
			//Get the Input
			Coin input = inputs.get(i);
			
			//Get the Coin Proof
			CoinProof cproof = mmrproofs.get(i);
			
			//Check Coin not already used..
			String coinid = cproof.getCoin().getCoinID().to0xString();
			if(allcoinsused.contains(coinid)) {
				MinimaLogger.log("CoinID used more than once @ "+i+" in "+zTxPoW.getTxPoWID());
				return false;
			}
			allcoinsused.add(coinid);
			
			//Check tokenid is correct
			if(!cproof.getCoin().getTokenID().isEqual(Token.TOKENID_MINIMA)) {
				
				//Check the token is correct
				if(!cproof.getCoin().getTokenID().isEqual(cproof.getCoin().getToken().getTokenID())) {
					MinimaLogger.log("TokenID in input "+i+" doesn't match token "+zTxPoW.getTxPoWID());
					return false;
				}
			}
			
			//Check the CoinProof details and Coin details Match
			boolean amount 	= input.getAmount().isEqual(cproof.getCoin().getAmount());
			boolean address = input.getAddress().isEqual(cproof.getCoin().getAddress());
			boolean token 	= input.getTokenID().isEqual(cproof.getCoin().getTokenID());
			if(!amount || !address || ! token) {
				MinimaLogger.log("Input coin doesn't match proof "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check the CoinProof and Coin CoinID in the Transaction Match
			if(input.getCoinID().isEqual(Coin.COINID_ELTOO)) {
				
				//Check is a floating input.. set when the coin was created!
				if(!cproof.getCoin().isFloating()) {
					MinimaLogger.log("ELTOO input "+i+" isn't floating "+zTxPoW.getTxPoWID());
					return false;
				}
				
			}else {
				
				//Check the same CoinID
				if(!input.getCoinID().isEqual(cproof.getCoin().getCoinID())) {
					MinimaLogger.log("CoinID input "+i+" doesn't match proof "+zTxPoW.getTxPoWID());
					return false;
				}
			}
			
			//Check the Coin Proof
			if(cproof.getCoin().getSpent()) {
				MinimaLogger.log("Trying to spend spent coin..");
				return false;
			}
			
			boolean validmmr = zTipMMR.checkProofTimeValid(cproof.getCoin().getMMREntryNumber(), cproof.getMMRData(), cproof.getMMRProof());
			if(!validmmr) {
				MinimaLogger.log("Invalid MMR Proof!");
				return false;
			}
			
			//Check the Script Proof
			ScriptProof prfs =  witness.getScript(input.getAddress());
			if(prfs == null) {
				MinimaLogger.log("Script Missing from TxPoW for address "+input.getAddress().to0xString());
				return false;
			}
			
			//Check the Script
			Contract contract = new Contract(prfs.getScript().toString(), witness.getAllSignatureKeys(), witness, transaction, input.getState());
			contract.setGlobals(zBlock, zTxPoW, i, cproof.getCoin().getBlockCreated(), prfs.getScript().toString());
			contract.run();
			if(!contract.isSuccess()) {
				MinimaLogger.log("Script FAIL "+prfs.getScript().toString());
				return false;
			}
			
			//Is there a token script..
			//..TODO
			
			//Is there a Burn Transaction
			//.. TODO
		}
		
		//All good
		return true;
	}
	
	public static boolean checkTxPoWScripts(MMR zTipMMR, TxPoW zTxPoW, MiniNumber zBlock) throws Exception {
		//Get the main Transaction..
		Transaction transaction = zTxPoW.getTransaction();
		
		//Get the Witness
		Witness witness = zTxPoW.getWitness();
		
		//Check the Inputs Coins..
		ArrayList<Coin> inputs 			= transaction.getAllInputs();
		int ins = inputs.size();
		
		//Get the coin proofs
		ArrayList<CoinProof> mmrproofs 	= witness.getAllCoinProofs();
		
		
		//Cycle through and check..
		for(int i=0;i<ins;i++) {
			
			//Get the Input
			Coin input = inputs.get(i);
			
			//Get the Coin Proof
			CoinProof cproof = mmrproofs.get(i);
			
			//Check the Script Proof
			ScriptProof prfs =  witness.getScript(input.getAddress());
			
			//Check the Script
			Contract contract = new Contract(prfs.getScript().toString(), witness.getAllSignatureKeys(), witness, transaction, input.getState());
			contract.setGlobals(zBlock, zTxPoW, i, cproof.getCoin().getBlockCreated(), prfs.getScript().toString());
			contract.run();
			if(!contract.isSuccess()) {
				MinimaLogger.log("Script FAIL "+prfs.getScript().toString());
				return false;
			}
			
			//Is there a token script..
			//..TODO
			
			//Is there a Burn Transaction
			//.. TODO
		}
		
		//All good
		return true;
	}
	
	/**
	 * Make basic checks of this TxPoW
	 */
	public static boolean checkTxPoWBasic(MMR zTipMMR, TxPoW zTxPoW, MiniNumber zBlock) throws Exception {
		//Get the main Transaction..
		Transaction transaction = zTxPoW.getTransaction();
		if(transaction.isEmpty()) {
			return true;
		}
		
		//Basic tests and Check Values add up..
		if(!transaction.checkValid()) {
			MinimaLogger.log("Invalid Transaction Inputs and Outputs.. "+transaction.toJSON());
			return false;
		}
		
		//Get the Witness
		Witness witness = zTxPoW.getWitness();
		
		//Check the Inputs Coins..
		ArrayList<Coin> inputs 			= transaction.getAllInputs();
		int ins = inputs.size();
		
		//Get  all the coin proofs..
		ArrayList<CoinProof> mmrproofs 	= witness.getAllCoinProofs();
		
		//Check we have the correct amount..
		if(ins != mmrproofs.size()) {
			MinimaLogger.log("MISSING MMR Proofs Inputs:"+ins+" MMRProofs:"+mmrproofs.size()+" @ "+zTxPoW.getTxPoWID());
			return false;
		}
		
		//List of CoinID used..
		ArrayList<String> allcoinsused = new ArrayList<>();
		
		//Cycle through and check..
		for(int i=0;i<ins;i++) {
			
			//Get the Input
			Coin input = inputs.get(i);
			
			//Get the Coin Proof
			CoinProof cproof = mmrproofs.get(i);
			
			//Check Coin not already used..
			String coinid = cproof.getCoin().getCoinID().to0xString();
			if(allcoinsused.contains(coinid)) {
				MinimaLogger.log("CoinID used more than once @ "+i+" in "+zTxPoW.getTxPoWID());
				return false;
			}
			allcoinsused.add(coinid);
			
			//Check tokenid is correct
			if(!cproof.getCoin().getTokenID().isEqual(Token.TOKENID_MINIMA)) {
				
				//Check the token is correct
				if(!cproof.getCoin().getTokenID().isEqual(cproof.getCoin().getToken().getTokenID())) {
					MinimaLogger.log("TokenID in input "+i+" doesn't match token "+zTxPoW.getTxPoWID());
					return false;
				}
			}
			
			//Check the CoinProof details and Coin details Match
			boolean amount 	= input.getAmount().isEqual(cproof.getCoin().getAmount());
			boolean address = input.getAddress().isEqual(cproof.getCoin().getAddress());
			boolean token 	= input.getTokenID().isEqual(cproof.getCoin().getTokenID());
			if(!amount || !address || ! token) {
				MinimaLogger.log("Input coin doesn't match proof "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check the CoinProof and Coin CoinID in the Transaction Match
			if(input.getCoinID().isEqual(Coin.COINID_ELTOO)) {
				
				//Check is a floating input.. set when the coin was created!
				if(!cproof.getCoin().isFloating()) {
					MinimaLogger.log("ELTOO input "+i+" isn't floating "+zTxPoW.getTxPoWID());
					return false;
				}
				
			}else {
				
				//Check the same CoinID
				if(!input.getCoinID().isEqual(cproof.getCoin().getCoinID())) {
					MinimaLogger.log("CoinID input "+i+" doesn't match proof "+zTxPoW.getTxPoWID());
					return false;
				}
			}
			
			//Check the Coin Proof
			if(cproof.getCoin().getSpent()) {
				MinimaLogger.log("Trying to spend spent coin..");
				return false;
			}
			
			//Check the Script Proofs exist for every coin
			ScriptProof prfs =  witness.getScript(input.getAddress());
			if(prfs == null) {
				MinimaLogger.log("Script Missing from TxPoW for address "+input.getAddress().to0xString());
				return false;
			}
		}
		
		//All good
		return true;
	}
	
	
	
	
	/**
	 * Only check the MMR Proofs
	 */
	public static boolean checkWitnessWMMR(MMR zTipMMR, TxPoW zTxPoW, MiniNumber zBlock) throws Exception {
		
		//Check the Transaction..
		boolean valid = checkWitnessWMMR(zTipMMR, zTxPoW.getWitness());
		if(!valid) {
			return false;
		}
		
		//Check the Burn Transaction..
		return checkWitnessWMMR(zTipMMR, zTxPoW.getBurnWitness());
	}
	
	private static boolean checkWitnessWMMR(MMR zTipMMR, Witness zWitness) throws Exception {
		//Get the all the MMR Proofs
		ArrayList<CoinProof> mmrproofs 	= zWitness.getAllCoinProofs();
		int proofs = mmrproofs.size();
		
		//Cycle through and check..
		for(int i=0;i<proofs;i++) {
			
			//Get the Coin Proof
			CoinProof cproof = mmrproofs.get(i);
			
			//Check the MMR
			boolean validmmr = zTipMMR.checkProofTimeValid(cproof.getCoin().getMMREntryNumber(), cproof.getMMRData(), cproof.getMMRProof());
			if(!validmmr) {
				MinimaLogger.log("Invalid MMR Proof!");
				return false;
			}
		}
		
		return true;
	}
	
	
	/**
	 * This is only done once when you first receive the TxPoW
	 */
	public static boolean checkSignatures(TxPoW zTxPoW) {
		
		//Get the Transaction Hash
		MiniData transid = zTxPoW.getTransID();
		
		//Now check the signatures..
		ArrayList<Signature> allsigs = zTxPoW.getWitness().getAllSignatures();
		for(Signature sig : allsigs) {
			
			//Create a signature scheme checker..
			TreeKey tk = new TreeKey();
			tk.setPublicKey(sig.getRootPublicKey());

			//Now check the sig..
			if(!tk.verify(transid, sig)) {
				MinimaLogger.log("SIGNATURE FAIL : "+zTxPoW.getTxPoWID());
				return false;
			}
		}
		
		return true;
	}
}
