package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.kissvm.Contract;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
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
	public static boolean checkTxPoWBlock(MMR zParentMMR, TxPoW zTxPoW, ArrayList<TxPoW> zTransactions) {
		//First check this
		if(zTxPoW.isTransaction()) {
			boolean valid = checkTxPoW(zParentMMR, zTxPoW, zTxPoW.getBlockNumber());
			if(!valid) {
				return false;
			}
		}
		
		//Now check all the internal Transactions
		for(TxPoW txpow : zTransactions) {
			boolean valid = checkTxPoW(zParentMMR, txpow, zTxPoW.getBlockNumber());
			if(!valid) {
				return false;
			}
		}
		
		//Construct the MMR.. to see if it is correct..
		TxBlock txblock 		= new TxBlock(zParentMMR, zTxPoW, zTransactions);
		TxPoWTreeNode node 		= new TxPoWTreeNode(txblock, false);

		//Check Correct..
		MMRData root = node.getMMR().getRoot();
		if(!root.getData().isEqual(zTxPoW.getMMRRoot()) || !root.getValue().isEqual(zTxPoW.getMMRTotal())) {
			MinimaLogger.log("ERROR : MMR in TxPOW and calculated don't match! @ "+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPoWID());
			return false;
		}
		
		return true;
	}
	
	public static boolean checkTxPoW(MMR zTipMMR, TxPoW zTxPoW, MiniNumber zBlock) {
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
		
		//Cycle through and check..
		for(int i=0;i<ins;i++) {
			
			//Get the Input
			Coin input = inputs.get(i);
			
			//Get the Coin Proof
			CoinProof cproof = mmrproofs.get(i);
			
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
		}
		
		//All good
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
