package org.minima.system.brains;

import java.util.ArrayList;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.cascade.CascadeNode;
import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.kissvm.Contract;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.Magic;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxBlock;
import org.minima.objects.TxHeader;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;

public class TxPoWChecker {

	/**
	 * What Network are we currently checking for
	 */
	public static MiniData CURRENT_NETWORK = TxHeader.TEST_NET;
	
	/**
	 * Parallel check all the transactions in this block
	 */
	public static boolean checkTxPoWBlock(TxPoWTreeNode zParentNode, TxPoW zTxPoW, ArrayList<TxPoW> zTransactions) {
		
		try {
			
			//Check ChainID
			if(!zTxPoW.getChainID().isEqual(TxPoWChecker.CURRENT_NETWORK)) {
				MinimaLogger.log("Invalid Block ChainID! "+zTxPoW.getChainID()+" "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check the Block Number is correct
			if(!zTxPoW.getBlockNumber().isEqual(zParentNode.getBlockNumber().increment())) {
				MinimaLogger.log("Invalid TxPoW block with wrong blocknumber "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check Parents..
			if(!checkParents(zParentNode, zTxPoW)) {
				MinimaLogger.log("Invalid TxPoW Super Parents "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check the block difficulty is correct
			MiniData blockdifficulty = TxPoWGenerator.getBlockDifficulty(zParentNode);
			if(!zTxPoW.getBlockDifficulty().isEqual(blockdifficulty)) {
				MinimaLogger.log("Incorrect TxPoW block difficulty "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check Magic numbers
			Magic txpowmagic = zTxPoW.getMagic();
			if(!txpowmagic.checkSame(zParentNode.getTxPoW().getMagic().calculateNewCurrent())) {
				MinimaLogger.log("Incorrect Magic values "+zTxPoW.getBlockTransactions().size()+" "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check Number of Txns..
			if(zTxPoW.getBlockTransactions().size() > txpowmagic.getMaxNumTxns().getAsInt()) {
				MinimaLogger.log("Too many transactions in block "+zTxPoW.getBlockTransactions().size()+" "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Check all the input coinid are Unique - use the MMR proofs! CoinID could be Eltoo
			ArrayList<String> allcoinid = new ArrayList<>();
			if(zTxPoW.isTransaction()) {
				//Main
				ArrayList<CoinProof> proofs = zTxPoW.getWitness().getAllCoinProofs();
				for(CoinProof proof : proofs) {
					allcoinid.add(proof.getCoin().getCoinID().to0xString());
				}
				
				//Burn
				proofs = zTxPoW.getBurnWitness().getAllCoinProofs();
				for(CoinProof proof : proofs) {
					allcoinid.add(proof.getCoin().getCoinID().to0xString());
				}
			}
			for(TxPoW txpow : zTransactions) {
				if(txpow.isTransaction()) {
					//Main
					ArrayList<CoinProof> proofs = txpow.getWitness().getAllCoinProofs();
					for(CoinProof proof : proofs) {
						allcoinid.add(proof.getCoin().getCoinID().to0xString());
					}
					
					//Burn
					proofs = txpow.getBurnWitness().getAllCoinProofs();
					for(CoinProof proof : proofs) {
						allcoinid.add(proof.getCoin().getCoinID().to0xString());
					}
				}
			}
			
			//Convert to unique Set and check equal size
			HashSet<String> coinset = new HashSet<>(allcoinid);
			if(coinset.size() != allcoinid.size()) {
				MinimaLogger.log("Invalid TxPoW Block with non unique CoinIDs "+zTxPoW.getTxPoWID());
				return false;
			}
			
			//Get the Parent MMR
			MMR parentMMR = zParentNode.getMMR();
			
			//First check this
			if(zTxPoW.isTransaction()) {
				boolean valid = checkTxPoWSimple(parentMMR, zTxPoW, zTxPoW);
				if(!valid) {
					return false;
				}
			}
			
			//Now check all the internal Transactions
			for(TxPoW txpow : zTransactions) {
				boolean valid = checkTxPoWSimple(parentMMR, txpow, zTxPoW);
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
	
	/**
	 * Make basic checks of this TxPoW
	 */
	public static boolean checkTxPoWBasic(TxPoW zTxPoW) throws Exception {
		
		//Check ChainID
		if(!zTxPoW.getChainID().isEqual(CURRENT_NETWORK)) {
			MinimaLogger.log("Wrong TxPoW ChainID! "+zTxPoW.getChainID()+" "+zTxPoW.getTxPoWID());
			return false;
		}
		
		//Check the Transaction..
		boolean valid = checkTxPoWBasic(zTxPoW.getTxPoWID(), zTxPoW.getTransaction(), zTxPoW.getWitness());
		if(!valid) {
			return false;
		}
		
		//Check the Link Hash
		if(!zTxPoW.getTransaction().getLinkHash().isEqual(MiniData.ZERO_TXPOWID)) {
			MinimaLogger.log("Invalid LinkHash for Transaction ( NOT 0x00 ) "+zTxPoW.getTxPoWID());
			return false;
		}
		
		//Is there a BURN transaction..
		if(!zTxPoW.getBurnTransaction().isEmpty()) {
			
			//Check the Link Hash
			if(!zTxPoW.getBurnTransaction().getLinkHash().isEqual(zTxPoW.getTransaction().getTransactionID())) {
				MinimaLogger.log("Invalid LinkHash for Burn Transaction "+zTxPoW.getTxPoWID());
				return false;
			}
		
			//Check the Burn Transaction..
			return checkTxPoWBasic(zTxPoW.getTxPoWID(), zTxPoW.getBurnTransaction(), zTxPoW.getBurnWitness());
		}
		
		return true;
	}
	
	private static boolean checkTxPoWBasic(String zTxPoWID, Transaction zTransaction, Witness zWitness) throws Exception {
		//Get the main Transaction..
		if(zTransaction.isEmpty()) {
			return true;
		}
		
		//Basic tests and Check Values add up..
		if(!zTransaction.checkValid()) {
			MinimaLogger.log("Invalid Transaction Inputs and Outputs.. "+zTransaction.toJSON());
			return false;
		}
		
		//Check the Inputs Coins..
		ArrayList<Coin> inputs 			= zTransaction.getAllInputs();
		int ins = inputs.size();
		
		//MUST be at least 1 input..
		if(ins==0) {
			MinimaLogger.log("Transaction MUST have at least 1 input @ "+zTxPoWID);
			return false;
		}
		
		//Get  all the coin proofs..
		ArrayList<CoinProof> mmrproofs 	= zWitness.getAllCoinProofs();
		
		//Check we have the correct amount..
		if(ins != mmrproofs.size()) {
			MinimaLogger.log("Wrong Number of MMR Proofs Inputs:"+ins+" MMRProofs:"+mmrproofs.size()+" @ "+zTxPoWID);
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
				MinimaLogger.log("CoinID used more than once @ "+i+" in "+zTxPoWID);
				return false;
			}
			allcoinsused.add(coinid);
			
			//Check the CoinProof details and Coin details Match
			boolean amount 	= input.getAmount().isEqual(cproof.getCoin().getAmount());
			boolean address = input.getAddress().isEqual(cproof.getCoin().getAddress());
			boolean token 	= input.getTokenID().isEqual(cproof.getCoin().getTokenID());
			if(!amount || !address || ! token) {
				MinimaLogger.log("Input coin details don't match coinproof "+zTxPoWID);
				return false;
			}
			
			//Check the CoinProof and Coin CoinID in the Transaction Match
			if(!input.getCoinID().isEqual(Coin.COINID_ELTOO)) {
				
				//Check the same CoinID
				if(!input.getCoinID().isEqual(cproof.getCoin().getCoinID())) {
					MinimaLogger.log("CoinID input "+i+" doesn't match proof "+zTxPoWID);
					return false;
				}
			}
			
			//Check token is correct
			if(!input.getTokenID().isEqual(Token.TOKENID_MINIMA)) {
				
				//Check the token is correct - in the coin
				if(!input.getTokenID().isEqual(input.getToken().getTokenID())) {
					MinimaLogger.log("TokenID in Coin input "+i+" doesn't match token "+zTxPoWID);
					return false;
				}
				
				//Check the token is correct - in the MMR
				if(!cproof.getCoin().getTokenID().isEqual(cproof.getCoin().getToken().getTokenID())) {
					MinimaLogger.log("TokenID in MMR Proof input "+i+" doesn't match token "+zTxPoWID);
					return false;
				}
			}
			
			//Check the Coin Proof
			if(cproof.getCoin().getSpent()) {
				MinimaLogger.log("Trying to spend spent coin..");
				return false;
			}
			
			//Check the Script Proofs exist for every coin
			ScriptProof prfs =  zWitness.getScript(input.getAddress());
			if(prfs == null) {
				MinimaLogger.log("Script Missing from TxPoW for address "+input.getAddress().to0xString());
				return false;
			}
		}
		
		//Check Output Tokens
		ArrayList<Coin> outputs = zTransaction.getAllOutputs();
		for(Coin cc : outputs) {
			if(!cc.getTokenID().isEqual(Token.TOKENID_MINIMA) && !cc.getTokenID().isEqual(Token.TOKENID_CREATE)) {
				
				if(cc.getToken() == null) {
					MinimaLogger.log("Incorrect output token with NULL token..");
					return false;
				
				}else if(!cc.getToken().getTokenID().isEqual(cc.getTokenID())) {
					
					MinimaLogger.log("Incorrect output token with different tokenid..");
					return false;
				}
			
			}else if(cc.getTokenID().isEqual(Token.TOKENID_CREATE)) {
				
				if(cc.getToken() == null) {
					MinimaLogger.log("Incorrect output token for create token with NULL token..");
					return false;
				}
			}
		}
		
		return true;
	}
	
	/**
	 * Once accepted basic and signature checks are no longer needed..
	 */
	public static boolean checkTxPoWSimple(MMR zTipMMR, TxPoW zTxPoW, TxPoW zBlock) throws Exception {
		
		//Check TxPoW is required Minimum..
		if(zTxPoW.getTxnDifficulty().isMore(zBlock.getMagic().getMinTxPowWork())) {
			MinimaLogger.log("TxPoW difficulty too low.. "+zTxPoW.getTxPoWID());
			return false;
		}
		
		//Check Size is acceptable..
		long size = zTxPoW.getSizeinBytesWithoutBlockTxns();
		if(size > zBlock.getMagic().getMaxTxPoWSize().getAsLong()) {
			MinimaLogger.log("TxPoW size too large.. "+size+" "+zTxPoW.getTxPoWID());
			return false;
		}
		
		//Check the MMR first - as quicker..
		boolean valid = checkMMR(zTipMMR, zTxPoW);
		if(!valid) {
			return false;
		}
		
		//Now check the scripts
		return checkTxPoWScripts(zTipMMR, zTxPoW, zBlock);
	}
	
	/**
	 * Check the Scripts of a transaction
	 */
	public static boolean checkTxPoWScripts(MMR zTipMMR, TxPoW zTxPoW, TxPoW zBlock) throws Exception {
		
		//Check the Transaction..
		boolean valid = checkTxPoWScripts(zTipMMR, zTxPoW.getTransaction(), zTxPoW.getWitness(), zBlock);
		if(!valid) {
			return false;
		}
		
		//Check the Burn Transaction..
		return checkTxPoWScripts(zTipMMR, zTxPoW.getBurnTransaction(), zTxPoW.getBurnWitness(), zBlock);
	}
	
	private static boolean checkTxPoWScripts(MMR zTipMMR, Transaction zTransaction, Witness zWitness, TxPoW zBlock) throws Exception {
		
		//Do we even need to check this!
		if(zTransaction.isCheckedMonotonic()) {
			return zTransaction.mIsValid;
		}
		
		//We are checking it now..
		zTransaction.mHaveCheckedMonotonic 	= true;
		zTransaction.mIsMonotonic 			= true;
		zTransaction.mIsValid 				= false;
		
		//Are we a valid transaction
		if(zTransaction.isEmpty()) {
			zTransaction.mIsValid = true;
			return true;
		}
		
		//Max KISSVM Ops
		int maxops = zBlock.getMagic().getMaxKISSOps().getAsInt();
		
		//Get the coin proofs
		ArrayList<CoinProof> mmrproofs 	= zWitness.getAllCoinProofs();
		int ins = mmrproofs.size();
		
		//Cycle through and check..
		for(int i=0;i<ins;i++) {
			
			//Get the Coin Proof
			CoinProof cproof = mmrproofs.get(i);
			
			//Check the Script Proof
			ScriptProof prfs =  zWitness.getScript(cproof.getCoin().getAddress());
			
			//Check the Script
			String script = prfs.getScript().toString();
			Contract contract = new Contract(script, 
											zWitness.getAllSignatureKeys(), 
											zWitness, 
											zTransaction, 
											cproof.getCoin().getState());
			
			contract.setMaxInstructions(maxops);
			contract.setGlobals(zBlock.getBlockNumber(), 
								zBlock.getTimeMilli(), 
								zTransaction, 
								i, 
								cproof.getCoin().getBlockCreated(), 
								script);
			contract.run();
			
			//Monotonic - no @BLKNUM references..
			if(!contract.isMonotonic()) {
				zTransaction.mIsMonotonic = false;
			}
			
			//Was it a success..
			if(!contract.isSuccess()) {
//				MinimaLogger.log("Script FAIL input:"+i+" "+contract.getCompleteTraceLog());
				MinimaLogger.log("Script FAIL input:"+i+" "+script);
				return false;
			}
			
			//Is there a token script..
			if(!cproof.getCoin().getTokenID().isEqual(Token.TOKENID_MINIMA)) {
				
				//Get the token..
				Token tok = cproof.getCoin().getToken();
				
				//Is it simple
				String tokscript = tok.getTokenScript().toString().trim();
				if(!tokscript.equals("RETURN TRUE")) {
					
					//Run it..
					Contract tokcontract = new Contract(tokscript, 
														zWitness.getAllSignatureKeys(), 
														zWitness, 
														zTransaction, 
														cproof.getCoin().getState());
					
					tokcontract.setMaxInstructions(maxops);
					tokcontract.setGlobals(	zBlock.getBlockNumber(), 
											zBlock.getTimeMilli(), 
											zTransaction, 
											i, 
											cproof.getCoin().getBlockCreated(), 
											tokscript);
					tokcontract.run();
					
					if(!tokcontract.isMonotonic()) {
						zTransaction.mIsMonotonic = false;
					}
					
					if(!tokcontract.isSuccess()) {
//						MinimaLogger.log("Token Script FAIL input:"+i+" "+tokcontract.getCompleteTraceLog());
						MinimaLogger.log("Token Script FAIL input:"+i+" "+tokscript);
						return false;
					}
				}
			}
		}
		
		//Transaction is valid
		zTransaction.mIsValid = true;
		
		return true;
	}
	
	/**
	 * Check the MMR Proofs
	 */
	public static boolean checkMMR(MMR zTipMMR, TxPoW zTxPoW) throws Exception {
		
		//Check the Transaction..
		boolean valid = checkMMR(zTipMMR, zTxPoW.getWitness());
		if(!valid) {
			return false;
		}
		
		//Check the Burn Transaction..
		return checkMMR(zTipMMR, zTxPoW.getBurnWitness());
	}
	
	private static boolean checkMMR(MMR zTipMMR, Witness zWitness) throws Exception {
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
	 * Check the signatures
	 */
	public static boolean checkSignatures(TxPoW zTxPoW) {
		//Check the Transaction..
		boolean valid = checkSignatures(zTxPoW, zTxPoW.getTransaction(), zTxPoW.getWitness());
		if(!valid) {
			return false;
		}
		
		//Check the Burn Transaction..
		return checkSignatures(zTxPoW, zTxPoW.getBurnTransaction(), zTxPoW.getBurnWitness());
	}
	
	private static boolean checkSignatures(TxPoW zTxPoW, Transaction zTransaction, Witness zWitness) {
		
		//Check the Main Transaction
		MiniData transid = zTransaction.getTransactionID();
		
		//Now check the signatures..
		ArrayList<Signature> allsigs = zWitness.getAllSignatures();
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
	
	/**
	 * Check coins for double spend in mempool
	 */
	public static boolean checkMemPoolCoins(TxPoW zTxPoW) {
		
		TxPoWDB txpdb = MinimaDB.getDB().getTxPoWDB();
		
		//Get all the coins..
		if(!zTxPoW.getTransaction().isEmpty()) {
			ArrayList<Coin> inputs = zTxPoW.getTransaction().getAllInputs();
			for(Coin cc : inputs) {
				if(txpdb.checkMempoolCoins(cc.getCoinID())) {
					return true;
				}
			}
		}
		
		if(!zTxPoW.getBurnTransaction().isEmpty()) {
			ArrayList<Coin> inputs = zTxPoW.getBurnTransaction().getAllInputs();
			for(Coin cc : inputs) {
				if(txpdb.checkMempoolCoins(cc.getCoinID())) {
					return true;
				}
			}
		}
		
		return false;
	}
	
	/**
	 * Check that all the Super Parent nodes are correct
	 */
	public static boolean checkParents(TxPoWTreeNode zTip, TxPoW zBlock) {
		
		//Cycle back through the chain..
		int blocksup 			= 0;
		TxPoWTreeNode current 	= zTip;
		while(current != null) {
			
			//Get the TxPoW
			TxPoW txpow 	= current.getTxPoW();
			MiniData txdata	= txpow.getTxPoWIDData();
			int superlevel 	= txpow.getSuperLevel();
			
			//Is it more than or equal to current required..
			while(superlevel>=blocksup) {
			
				//The current super parent of the block
				MiniData superparent = zBlock.getSuperParent(blocksup);
				
				//Make sure is valid..
				if(!superparent.isEqual(txdata)) {
					return false;
				}
				
				blocksup++;
			}
			
			current = current.getParent();
		}
		
		//Now go through the cascade
		CascadeNode cnode = MinimaDB.getDB().getCascade().getTip();
		while(cnode != null) {
			
			//Get the TxPoW
			TxPoW txpow 	= cnode.getTxPoW();
			MiniData txdata	= txpow.getTxPoWIDData();
			int superlevel 	= txpow.getSuperLevel();
			
			//Is it more than or equal to current required..
			while(superlevel>=blocksup) {
			
				//The current super parent of the block
				MiniData superparent = zBlock.getSuperParent(blocksup);
				
				//Make sure is valid..
				if(!superparent.isEqual(txdata)) {
					return false;
				}
				
				blocksup++;
			}
			
			cnode = cnode.getParent();
		}
		
		//Check that the remaining all point to 0x00
		for(int i=blocksup;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			if(!zBlock.getSuperParent(blocksup).isEqual(MiniData.ZERO_TXPOWID)) {
				return false;
			}
		}
		
		return true;
	}
}
