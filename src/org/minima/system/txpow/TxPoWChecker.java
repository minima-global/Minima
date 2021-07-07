package org.minima.system.txpow;

import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.kissvm.Contract;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Magic;
import org.minima.objects.StateVariable;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.MultiKey;
import org.minima.objects.proofs.ScriptProof;
import org.minima.objects.proofs.SignatureProof;
import org.minima.system.input.functions.gimme50;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class TxPoWChecker {
	
	/**
	 * Check JUST the signatures. This only ever has to be done once.
	 * 
	 * @param zTxPOW
	 * @return
	 */
	public static boolean checkSigs(TxPoW zTxPOW) {
		//Only if it has a body
		if(!zTxPOW.hasBody()) {
			return true;
		}
		
		//get the Transaction..
		Transaction trans = zTxPOW.getTransaction();
		
		//Get the Hash
		MiniData transhash = zTxPOW.getTransID();
		
		//Now cycle
		Witness wit = zTxPOW.getWitness();
		
		//Get all the signatures..
		ArrayList<SignatureProof> sigs = wit.getAllSignatures();
		
		//Check each one and add.. this is only done once..
		for(SignatureProof sig : sigs) {
			//This is the actual public key that is being represented..
//			MiniData leafkey = sig.getFinalHash();
			
			//Now check the leaf of the tree
			MiniData leafkey   = sig.getData();
			MiniData signature = sig.getSignature();
		
			//Create a MultiKey to check the signature
			MultiKey checker = new MultiKey();
			checker.setPublicKey(leafkey);
			
			//Check it..
//			boolean ok = PubPrivKey.verify(leafkey, transhash, signature);
			boolean ok = checker.verify(transhash, signature);
			
			//Check it..
//			boolean ok = PubPrivKey.verify(leafkey, transhash, signature);
			if(!ok) {
				return false;
			}
		}
		
		return true;
	}
	
	/**
	 * Perform Basic checks on a complete TxPoW message
	 * @param txpow
	 * @return if this is valid
	 */
	public static boolean basicTxPowChecks(TxPoW txpow) {
		//Must be at least a block or a transaction..
		if(!txpow.isBlock() && !txpow.isTransaction()) {
			MinimaLogger.log("ERROR NET FAKE - not transaction not block : "+txpow.getBlockNumber()+" "+txpow);
			return false;
		}
		
		//Is the Transaction PoWerful enough..
		if(txpow.isTransaction() && txpow.getTxnDifficulty().isMore(Magic.MIN_TXPOW_WORK)) {
			MinimaLogger.log("ERROR NET - Transaction not enough TxPOW: "+txpow.getTxnDifficulty()+" "+txpow);
			return false;
		}
		
		//Check the inputs and outputs are valid
		if(txpow.isTransaction() && !txpow.getTransaction().checkValidInOutPerToken()) {
			MinimaLogger.log("ERROR NET - Transaction inputs not unique or less than outputs "+txpow);
			return false;
		}
		
		//Does it have a body.. SHOULD NOT HAPPEN as only complete post cascade txpow messages can be requested
		if(!txpow.hasBody()) {
			MinimaLogger.log("ERROR NET NO TxBODY for txpow "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
			return false;
		}
		
		//Check Header and Body Agree..
		MiniData bodyhash = Crypto.getInstance().hashObject(txpow.getTxBody());
		if(!txpow.getTxHeader().getBodyHash().isEqual(bodyhash)) {
			MinimaLogger.log("ERROR NET TxHeader and TxBody Mismatch! "+txpow.getBlockNumber()+" "+txpow.getTxPowID()+" "+txpow.getTxHeader().getBodyHash().to0xString()+" "+bodyhash.to0xString()); 
			return false;
		}
		
		//Check the Signatures.. just the once..
		boolean sigsok = TxPoWChecker.checkSigs(txpow);
		if(!sigsok) {
			MinimaLogger.log("ERROR NET Invalid Signatures with TXPOW : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
			return false;
		}
		
		return true;
	}
	
	/**
	 * Check a transaction, and update the MMR. If the block is invalid - the MMR will never be used anyway.
	 * @param zTrans
	 * @param zWit
	 * @param zDB
	 * @param zBlockNumber - current block number
	 * @param zMMRSet
	 * @return
	 */
	public static boolean checkTransactionMMR(TxPoW zTxPOW, MinimaDB zDB) {
		//And use the chaintip for all the parameters..
		BlockTreeNode tip = zDB.getMainTree().getChainTip();
		TxPoW block       = tip.getTxPow();
		
		return checkTransactionMMR(zTxPOW, zDB, block, tip.getMMRSet(), false);
	}
	
	public static boolean checkTransactionMMR(TxPoW zTxPOW, MinimaDB zDB, 
			TxPoW zBlock, MMRSet zMMRSet, boolean zTouchMMR) {
		return checkTransactionMMR(zTxPOW, zDB, zBlock, zMMRSet, zTouchMMR, new JSONArray());
	}
	
	public static boolean checkTransactionMMR(TxPoW zTxPOW, MinimaDB zDB, 
				TxPoW zBlock, MMRSet zMMRSet, boolean zTouchMMR, JSONArray zContraclogs) {
		
		JSONObject contractlog = new JSONObject();
		
		//need a body
		if(!zTxPOW.hasBody()) {
			contractlog.put("error", "TxPoW has no body!");
			contractlog.put("txpow", zTxPOW.toJSON());
			zContraclogs.add(contractlog);
			return false;
		}
		
		//Now Check the Transaction Link Hash..
		if(!zTxPOW.getTransaction().getLinkHash().isEqual(new MiniData("0x00"))) {
			contractlog.put("error", "LinkHash not equal to 0x00");
			contractlog.put("txpow", zTxPOW.toJSON());
			zContraclogs.add(contractlog);
			return false;
		}
		
		return checkTransactionMMR(zTxPOW.getTransaction(), zTxPOW.getWitness(), zDB, zBlock, zMMRSet, zTouchMMR, zContraclogs);	
	}
	
	public static boolean checkTransactionMMR(Transaction zTrans, Witness zWit, MinimaDB zDB, 
			TxPoW zBlock, MMRSet zMMRSet, boolean zTouchMMR, JSONArray zContractLog) {
		
		//Empty Transaction passes..
		if(zTrans.isEmpty()) {
			return true;
		}
		
		//get some extra variables..
		MiniNumber tBlockNumber = zBlock.getBlockNumber();
		MiniNumber tBlockTime   = zBlock.getTimeMilli();
		
		//Make a deep copy.. as we may need to edit it.. with floating values
		Transaction trans;
		try {
			trans = zTrans.deepCopy();
		} catch (IOException e) {
			MinimaLogger.log("Error deep copy Transaction "+zTrans.toString());
			return false;
		}
		
		//Check the input scripts
		ArrayList<Coin> inputs  = trans.getAllInputs();
		
		//The Signatures
		String sigs = zWit.getAllPubKeysCSV();
				
		//First Inputs..
		int ins = inputs.size();
		for(int i=0;i<ins;i++) {
			//Get the Input
			Coin input = inputs.get(i);
			
			//The contract execution log - will be updated later, but added now
			JSONObject contractlog = new JSONObject();
			zContractLog.add(contractlog);
			
			//Get the Script..
			ScriptProof sp =  zWit.getScript(input.getAddress());
			if(sp == null) {
				contractlog.put("error", "Script not found for "+input.getAddress());
				return false;
			}
			
			//the script for this input
			String script = sp.getScript().toString();
			
			contractlog.put("input", i);
			contractlog.put("script", script);
			
			if(input.getCoinID().isEqual(gimme50.COINID_INPUT) && input.getAmount().isLessEqual(new MiniNumber("50"))){
				//We good.. TESTNET allows up to 50 printed..
				//..
				contractlog.put("isgimme50", true);
			}else {
				contractlog.put("isgimme50", false);
				
				//Check the Address is the hash of the SCRIPT
				Address scraddr = new Address(script,input.getAddress().getLength()*8);
				if(!scraddr.getAddressData().isEqual(input.getAddress())) {
					contractlog.put("error", "Serious - Invalid Address for script!");
					return false;
				}
				
				//Is it a valid input.. UNSPENT in MMR
				MMRProof proof = zWit.getAllMMRProofs().get(i);
				
				//MUST be a full proof - this done in checkproof..
				if(proof.getMMRData().isHashOnly()) {
					contractlog.put("error", "Invalid MMR Proof (HASH Only)");
					return false;
				}
				
				//Is the proof chain valid
				boolean valid = zMMRSet.checkProof(proof);
				if(!valid) {
					contractlog.put("error", "Invalid MMR Proof");
					return false;
				}
				
				//Is it floating input.. set the COinID..
				if(input.isFloating()) {
					Coin flinput = proof.getMMRData().getCoin();
					input.resetCoinID(flinput.getCoinID());
				}
				
				//Check the Coin is Correct..
				if(!proof.checkCoin(input)) {
					contractlog.put("error", "Coin details proof miss-match");
					return false;
				}
				
				//Is this a Token ?
				String tokscript = "";
				MiniNumber tokentotal = MiniNumber.BILLION;
//				MiniNumber tokenscale = MiniNumber.ONE;
				MiniNumber tamount = input.getAmount();
				
				if(!input.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
					//Do we have a token Script..
					Token tokdets = zWit.getTokenDetail(input.getTokenID());
					
					if(tokdets == null) {
						contractlog.put("error", "Token Details for coin missing! "+input.getTokenID());
						return false;	
					}
					
					//Is there a script.
					tokscript  = tokdets.getTokenScript().toString();
//					tokenscale = tokdets.getScaleFactor();
					tamount    = tokdets.getScaledTokenAmount(tamount);
					tokentotal = tokdets.getTotalTokens();
				}
				
				//Create the Contract to check..
				Contract cc = new Contract(script, sigs, zWit, trans,proof.getMMRData().getPrevState());
				
				//Set MAX instructions
				cc.setMaxInstructions(zBlock.getMagic().getMaxKISSInst());
				
				//set the environment
				cc.setGlobalVariable("@BLKNUM", new NumberValue(tBlockNumber));
				cc.setGlobalVariable("@BLKTIME", new NumberValue(tBlockTime));
				cc.setGlobalVariable("@BLKDIFF", new NumberValue(tBlockNumber.sub(proof.getMMRData().getInBlock())));
				cc.setGlobalVariable("@PREVBLKHASH", new HexValue(zBlock.getParentID()));
				cc.setGlobalVariable("@INBLKNUM", new NumberValue(proof.getMMRData().getInBlock()));
				cc.setGlobalVariable("@INPUT", new NumberValue(i));
				cc.setGlobalVariable("@AMOUNT", new NumberValue(tamount));
				cc.setGlobalVariable("@ADDRESS", new HexValue(input.getAddress()));
				cc.setGlobalVariable("@COINID", new HexValue(input.getCoinID()));
				cc.setGlobalVariable("@SCRIPT", new StringValue(script));
				cc.setGlobalVariable("@TOKENID", new HexValue(input.getTokenID()));
				cc.setGlobalVariable("@TOKENSCRIPT", new StringValue(tokscript));
				cc.setGlobalVariable("@TOKENTOTAL", new NumberValue(tokentotal));
				cc.setGlobalVariable("@FLOATING", new BooleanValue(input.isFloating()));
				cc.setGlobalVariable("@TOTIN", new NumberValue(trans.getAllInputs().size()));
				cc.setGlobalVariable("@TOTOUT", new NumberValue(trans.getAllOutputs().size()));
									
				//Is it a floating coin..
				cc.setFloating(input.isFloating());
						
				//Run it!
				cc.run();
				
				//Contract Execution details
				contractlog.put("script", cc.getMiniScript());
				contractlog.put("size", cc.getMiniScript().length());
				contractlog.put("instructions", cc.getNumberOfInstructions());
				contractlog.put("address", input.getAddress().to0xString());
				contractlog.put("parseok", cc.isParseOK());
				contractlog.put("parse", cc.getCompleteTraceLog());
				contractlog.put("exception", cc.isException());
				contractlog.put("excvalue", cc.getException());
				contractlog.put("result", cc.isSuccess());
				
				//and.. ?
				if(!cc.isSuccess()) {
					return false;
				}
				
				//Is this a Token ?
				if(!input.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
					//Complex Script ?
					if(!tokscript.equals("RETURN TRUE")) {
						//Check the Token Script!
						Contract tokencc = new Contract(tokscript, sigs, zWit, trans, proof.getMMRData().getPrevState());
				
						//Set MAX instructions
						cc.setMaxInstructions(zBlock.getMagic().getMaxKISSInst());
						
						//set the environment - same as the first contract
						tokencc.setAllGlobalVariables(cc.getGlobalVariables());
						
						//Is it a floating coin..
						tokencc.setFloating(input.isFloating());
						
						//Run it!
						tokencc.run();
						
						//Log it all
						JSONObject toklog = new JSONObject();
						contractlog.put("tokencontract", toklog);
						
						//Token Contract execution details
						toklog.put("script", tokencc.getMiniScript());
						toklog.put("size", tokencc.getMiniScript().length());
						toklog.put("instructions", tokencc.getNumberOfInstructions());
						toklog.put("address", input.getAddress().to0xString());
						toklog.put("parseok", tokencc.isParseOK());
						toklog.put("parse", tokencc.getCompleteTraceLog());
						toklog.put("exception", tokencc.isException());
						toklog.put("excvalue", tokencc.getException());
						toklog.put("result", tokencc.isSuccess());
						
						//and.. ?
						if(!tokencc.isSuccess()) {
							return false;
						}
					}
				}
			}
		}
				
		//Check output token details exist
		ArrayList<Coin> outputs  = trans.getAllOutputs();
		int outs = outputs.size();
		for(int i=0;i<outs;i++) {
			//Get the coin..
			Coin output     = outputs.get(i);
			
			//Is this a token create output..
			MiniData tokid 	= output.getTokenID();
			
			if(!tokid.isEqual(Coin.MINIMA_TOKENID) && !tokid.isEqual(Coin.TOKENID_CREATE)) {
				//Check it..
				if(zWit.getTokenDetail(tokid) == null) {
					//The contract execution log - will be updated later, but added now
					JSONObject errorlog = new JSONObject();
					zContractLog.add(errorlog);
					errorlog.put("error", "Token Details Missing for "+tokid);
					return false;
				}
			}
		}
		
		//ONLY NOW - Touch MMR and Add All KNOWN Tokens..
		if(zTouchMMR) {
			//Is the STATE relevant.. does it have a KEY we own..
			boolean relstate = zDB.getUserDB().isStateListRelevant(trans.getCompleteState());
					
			//The HASH of the Transaction.. needed for coinid
			//The transaction may have been altered by floating inputs..
			MiniData transhash = Crypto.getInstance().hashObject(trans);
			
			//First Update all the inputs..
			for(int i=0;i<ins;i++) {
				//Get the Input
				Coin input = inputs.get(i);
				
				//Not for Gimme50
				if(!input.getCoinID().isEqual(gimme50.COINID_INPUT)){
					//Get the Proof
					MMRProof proof = zWit.getAllMMRProofs().get(i);
					
					//Update the MMR with this spent coin..
					MMREntry spent  = zMMRSet.updateSpentCoin(proof);
		
					//Do we keep it..
					if(zDB.getUserDB().isAddressRelevant(input.getAddress())) {
						zMMRSet.addKeeper(spent.getEntryNumber());	
					}
				}
			}
			
			//Now update all the outputs..
			for(int i=0;i<outs;i++) {
				//Get the coin..
				Coin output = outputs.get(i);
			
				//Now calculate the CoinID / TokenID
				MiniData coinid = Crypto.getInstance().hashObjects(transhash, new MiniByte(i));
				
				//Is this a token create output..
				MiniData tokid 	= output.getTokenID();

				//Is there a new token
				Token newtokdets = null;
				
				//Is this a token or are we creating a Token
				if(tokid.isEqual(Coin.TOKENID_CREATE)) {
					//Make it the HASH ( CoinID | Total Amount..the token details )
					Token gentoken = trans.getTokenGenerationDetails();
					newtokdets = new Token( coinid, 
											gentoken.getScale(), 
											gentoken.getAmount(), 
											gentoken.getName(), 
											gentoken.getTokenScript());
					
					//Set the Globally Unique TokenID!
					tokid = newtokdets.getTokenID();
					
					//Its a regular token transaction
				}else if(!tokid.isEqual(Coin.MINIMA_TOKENID)) {
					//Get the token..
					newtokdets = zWit.getTokenDetail(tokid);
				
				}else {
					//It's Minima..
					newtokdets = null;
				}
				
				//Create a new Coin..
				Coin mmrcoin = new Coin(coinid, output.getAddress(), output.getAmount(), tokid, output.isFloating(), output.storeState());
				
				//Create the MMRData and see if we store the state.. 
				MMRData mmrdata = null;
				if(output.storeState()) {
					mmrdata = new MMRData(false, mmrcoin, newtokdets, tBlockNumber, trans.getCompleteState());
				}else {
					mmrdata = new MMRData(false, mmrcoin, newtokdets, tBlockNumber, new ArrayList<StateVariable>());
				}
				
				//And Add it..
				MMREntry unspent = zMMRSet.addUnspentCoin(mmrdata);
				
				//Do we keep this output..
				boolean reladdress = zDB.getUserDB().isCoinRelevant(output);
				
				//Do we keep it..
				if( reladdress || ( relstate && output.storeState() ) ) {
					//Keep this MMR record
					zMMRSet.addKeeper(unspent.getEntryNumber());	
				}	
			}
		}
				
		//All OK!
		return true;
	}
}
