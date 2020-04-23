package org.minima.system.brains;

import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.kissvm.Contract;
import org.minima.kissvm.functions.state.DYNSTATE;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.ScriptProof;
import org.minima.objects.proofs.SignatureProof;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.input.functions.gimme50;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class TxPOWChecker {
	
	/**
	 * Check JUST the signatures. This only ever has to be done once.
	 * 
	 * @param zTxPOW
	 * @return
	 */
	public static boolean checkSigs(TxPOW zTxPOW) {
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
		
			//Check it..
			boolean ok = PubPrivKey.verify(leafkey, transhash, signature);
			if(!ok) {
				return false;
			}
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
	public static boolean checkTransactionMMR(TxPOW zTxPOW, MinimaDB zDB) {
		//And use the chaintip for all the parameters..
		BlockTreeNode tip = zDB.getMainTree().getChainTip();
		return checkTransactionMMR(zTxPOW, zDB, tip.getTxPow().getBlockNumber(), tip.getMMRSet(), false);
	}
	
	public static boolean checkTransactionMMR(TxPOW zTxPOW, MinimaDB zDB, MiniNumber zBlockNumber, MMRSet zMMRSet, boolean zTouchMMR) {
		//Burn Transaction check!.. 
		if(!zTxPOW.getBurnTransaction().isEmpty()) {
			//Get MAIN Transaction Hash - make sure is correct in Burn Transaction
			MiniData transid = zTxPOW.getTransID();
			
			//Check is correct on Burn Transaction..
			if(!zTxPOW.getBurnTransaction().getLinkHash().isEqual(transid)) {
				return false;
			}
			
			boolean burntrans = checkTransactionMMR(zTxPOW.getBurnTransaction(), 
													zTxPOW.getBurnWitness(), 
													zDB, zBlockNumber, zMMRSet, zTouchMMR, 
													new JSONArray());
			if(!burntrans) {
				return false;
			}
		}
		
		//Now Check the Transaction Link Hash..
		if(!zTxPOW.getTransaction().getLinkHash().isEqual(new MiniData("0x00"))) {
			return false;
		}
		
		return checkTransactionMMR(zTxPOW.getTransaction(), zTxPOW.getWitness(), zDB, zBlockNumber, zMMRSet, zTouchMMR, new JSONArray());	
	}
	
	public static boolean checkTransactionMMR(Transaction zTrans, Witness zWit, MinimaDB zDB, MiniNumber zBlockNumber, MMRSet zMMRSet, boolean zTouchMMR) {
		return checkTransactionMMR(zTrans, zWit, zDB, zBlockNumber, zMMRSet, zTouchMMR, new JSONArray());	
	}
	
	public static boolean checkTransactionMMR(Transaction zTrans, Witness zWit, MinimaDB zDB, MiniNumber zBlockNumber, MMRSet zMMRSet, boolean zTouchMMR, JSONArray zContractLog) {
		//Make a deep copy.. as we may need to edit it.. with floating values and DYN_STATE
		Transaction trans = zTrans.deepCopy();
		
		//The DYNState variables..
		String[] DYNState    = new String[256];
		boolean[] checkState = new boolean[256];
		for(int i=0;i<256;i++) {
			DYNState[i]   = null;
			checkState[i] = false;
		}
		
		//Check the input scripts
		ArrayList<Coin> inputs  = trans.getAllInputs();
		
		//The Signatures
		String sigs = zWit.getAllPubKeysCSV();
		
		//If ANY of the inputs are floating.. check for remainder outputs.
		boolean isfloating = false;

		
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
					//Are we a floating input.. ?
					if(input.isFloating()) {
						//See if there is a valid address/amount..
						MMREntry fladdr = zMMRSet.searchAddress(input.getAddress(), input.getAmount(), input.getTokenID());
						if(fladdr != null) {
							//There is a valid coin  we can use..!
							proof = zMMRSet.getProof(fladdr.getEntry());	
							
							//Now CHANGE the Transaction with this new CoinID AND AMOUNT..
							Coin flinput = proof.getMMRData().getCoin();
							input.resetCoinID(flinput.getCoinID());
							input.resetAmount(flinput.getAmount());
							
							//And you may have to change the remainder output..
							isfloating = true;
								
						}else {
							contractlog.put("error", "Invalid MMR Proof and NO VALID FLOATING COIN Found..");
							return false;
						}
					}else {
						contractlog.put("error", "Invalid MMR Proof");
						return false;	
					}
				}else {
					//Is this input for the correct details..
					if(!proof.checkCoin(input)) {
						contractlog.put("error", "Coin details proof miss-match");
						return false;
					}	
				}
				
				if(zTouchMMR) {
					//Update the MMR with this spent coin..
					MMREntry spent = zMMRSet.updateSpentCoin(proof);
				
					//Do we keep it..
					if(zDB.getUserDB().isAddressRelevant(input.getAddress())) {
						zMMRSet.addKeeper(spent.getEntry());	
					}
				}
				
				//Is this a Token ?
				String tokscript = "";
				if(!input.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
					//Do we have a token Script..
					TokenProof tokdets = zWit.getTokenDetail(input.getTokenID());
					
					if(tokdets == null) {
						contractlog.put("error", "Token Details for coin missing! "+input.getTokenID());
						return false;	
					}
					
					//Is there a script.
					tokscript = tokdets.getTokenScript().toString();
				}
				
				//Create the Contract to check..
				Contract cc = new Contract(script,sigs, zWit, trans,proof.getMMRData().getPrevState());
				
				//set the environment
				cc.setGlobalVariable("@BLKNUM", new NumberValue(zBlockNumber));
				cc.setGlobalVariable("@INBLKNUM", new NumberValue(proof.getMMRData().getInBlock()));
				cc.setGlobalVariable("@BLKDIFF", new NumberValue(zBlockNumber.sub(proof.getMMRData().getInBlock())));
				cc.setGlobalVariable("@INPUT", new NumberValue(i));
				cc.setGlobalVariable("@AMOUNT", new NumberValue(input.getAmount()));
				cc.setGlobalVariable("@ADDRESS", new HEXValue(input.getAddress()));
				cc.setGlobalVariable("@TOKENID", new HEXValue(input.getTokenID()));
				cc.setGlobalVariable("@COINID", new HEXValue(input.getCoinID()));
				cc.setGlobalVariable("@SCRIPT", new ScriptValue(script));
				cc.setGlobalVariable("@TOKENSCRIPT", new ScriptValue(tokscript));
				cc.setGlobalVariable("@FLOATING", new BooleanValue(input.isFloating()));
				cc.setGlobalVariable("@TOTIN", new NumberValue(trans.getAllInputs().size()));
				cc.setGlobalVariable("@TOTOUT", new NumberValue(trans.getAllOutputs().size()));
				
				//Is it a floating coin..
				cc.setFloating(input.isFloating());
				
				//Set the DYNState..
				cc.setCompleteDYNState(DYNState,checkState);
				
				//Run it!
				cc.run();
				
				contractlog.put("script", cc.getMiniScript());
				contractlog.put("size", cc.getMiniScript().length());
				contractlog.put("instructions", cc.getNumberOfInstructions());
				contractlog.put("address", input.getAddress().to0xString());
				contractlog.put("parseok", cc.isParseOK());
				contractlog.put("parse", cc.getCompleteTraceLog());
				contractlog.put("exception", cc.isException());
				contractlog.put("result", cc.isSuccess());
				
				//Get the DynState
				DYNState   = cc.getCompleteDYNState();
				checkState = cc.getCompleteCheckState();
				
				//and.. ?
				if(!cc.isSuccess()) {
					return false;
				}
				
				//Is this a Token ?
				if(!input.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
					//Complex Script ?
					if(!tokscript.equals("RETURN TRUE")) {
						//Check the Script!
						cc = new Contract(tokscript,sigs, zWit, trans,proof.getMMRData().getPrevState());
						
						//set the environment
						cc.setGlobalVariable("@BLKNUM", new NumberValue(zBlockNumber));
						cc.setGlobalVariable("@INBLKNUM", new NumberValue(proof.getMMRData().getInBlock()));
						cc.setGlobalVariable("@BLKDIFF", new NumberValue(zBlockNumber.sub(proof.getMMRData().getInBlock())));
						cc.setGlobalVariable("@INPUT", new NumberValue(i));
						cc.setGlobalVariable("@AMOUNT", new NumberValue(input.getAmount()));
						cc.setGlobalVariable("@ADDRESS", new HEXValue(input.getAddress()));
						cc.setGlobalVariable("@TOKENID", new HEXValue(input.getTokenID()));
						cc.setGlobalVariable("@COINID", new HEXValue(input.getCoinID()));
						cc.setGlobalVariable("@SCRIPT", new ScriptValue(script));
						cc.setGlobalVariable("@TOKENSCRIPT", new ScriptValue(tokscript));
						cc.setGlobalVariable("@FLOATING", new BooleanValue(input.isFloating()));
						cc.setGlobalVariable("@TOTIN", new NumberValue(trans.getAllInputs().size()));
						cc.setGlobalVariable("@TOTOUT", new NumberValue(trans.getAllOutputs().size()));
						
						//Is it a floating coin..
						cc.setFloating(input.isFloating());
						
						//Set the DYNState..
						cc.setCompleteDYNState(DYNState,checkState);
						
						//Run it!
						cc.run();
						
						//Get the DynState
						DYNState   = cc.getCompleteDYNState();
						checkState = cc.getCompleteCheckState();
									
						JSONObject toklog = new JSONObject();
						contractlog.put("tokencontract", toklog);
						
						toklog.put("script", cc.getMiniScript());
						toklog.put("size", cc.getMiniScript().length());
						toklog.put("instructions", cc.getNumberOfInstructions());
						toklog.put("address", input.getAddress().to0xString());
						toklog.put("parseok", cc.isParseOK());
						toklog.put("parse", cc.getCompleteTraceLog());
						toklog.put("exception", cc.isException());
						toklog.put("result", cc.isSuccess());
						
						//and.. ?
						if(!cc.isSuccess()) {
							return false;
						}
					}
				}
			}
		}
		
		//Do we need to check the Remainders - Reset the amount if a 
		//floating coin has changed the input amounts.. This will only ever be MORE..
		if(isfloating) {
			ArrayList<String> tokens = new ArrayList<>();
			for(Coin cc : trans.getAllInputs()) {
				String tok = cc.getTokenID().to0xString();
				if(!tokens.contains(tok)) {
					tokens.add(tok);	
				}
			}
			
			//Now get all the Input Amounts...
			Hashtable<String, MiniNumber> inamounts = new Hashtable<>();
			for(String token : tokens) {
				inamounts.put(token, trans.sumInputs(new MiniData(token)));
			}
			
			//Now get the output amounts..
			for(String token : tokens) {
				MiniData tok = new MiniData(token);
				
				//Summthe outputs for this token type
				MiniNumber outamt = trans.sumOutputs(tok);
				
				//Do we need to reset the remainder amount - if one exists ?
				MiniNumber inamt = inamounts.get(token);
				
				//Calculate the remainder
				MiniNumber remainder = inamt.sub(outamt);
				
				//OK - some resetting to do..
				if(!remainder.isEqual(MiniNumber.ZERO)) {
					//Find the Remainder output for this token type..
					Coin remainderoutput = trans.getRemainderCoin(tok);
					
					if(remainderoutput != null) {
						//Reset the output amount..
						remainderoutput.resetAmount(remainderoutput.getAmount().add(remainder));	
					}
				}
			}
			
			//Reset any changed DYNSTATE
			for(int i=0;i<256;i++) {
				if(DYNState[i] != null) {
					//Set it..
					trans.addStateVariable(new StateVariable(i, DYNState[i]));
				}
			}
		}
		
		//Is the STATE relevant.. does it have a KEY we own..
		boolean relstate = zDB.getUserDB().isStateListRelevant(trans.getCompleteState());
				
		//The HASH of the Transaction.. needed for coinid
		//The transaction may have been altered by floating inputs..
		MiniData transhash = Crypto.getInstance().hashObject(trans);
				
		//Get outputs - add them to the MMR also..
		TokenProof newtokdets = null;
		ArrayList<Coin> outputs  = trans.getAllOutputs();
		int outs = outputs.size();
		for(int i=0;i<outs;i++) {
			//Get the coin..
			Coin output = outputs.get(i);
			
			//Now calculate the CoinID / TokenID
			MiniData coinid = Crypto.getInstance().hashObjects(transhash, new MiniByte(i));
			
			//Is this a token create output..
			MiniData tokid 			= output.getTokenID();
			
			//Is this a token or are we creating a Token
			if(tokid.isEqual(Coin.TOKENID_CREATE)) {
				//Make it the HASH ( CoinID | Total Amount..the token details )
				TokenProof gentoken = trans.getTokenGenerationDetails();
				newtokdets = new TokenProof(coinid, 
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
				
				//Check it..
				if(newtokdets == null) {
					//The contract execution log - will be updated later, but added now
					JSONObject errorlog = new JSONObject();
					zContractLog.add(errorlog);
					errorlog.put("error", "Total Details Missing for "+tokid);
					return false;
				}
			}
	
			//Are we writing to the MMR
			if(zTouchMMR) {
				//Create a new Coin..
				Coin mmrcoin = new Coin(coinid, output.getAddress(), output.getAmount(), tokid);
				
				//Now add as an unspent to the MMR
				MMRData mmrdata = new MMRData(MiniByte.FALSE, mmrcoin, zBlockNumber, trans.getCompleteState());
				
				//And Add it..
				MMREntry unspent = zMMRSet.addUnspentCoin(mmrdata);
				
				//Do we keep this output..
				boolean reladdress = zDB.getUserDB().isAddressRelevant(output.getAddress());
//				if(!rel) {
//					rel = zDB.getUserDB().isStateListRelevant(trans.getCompleteState());
//				}
				
				//Do we keep it..
				if(reladdress || relstate) {
					//Keep this MMR record
					zMMRSet.addKeeper(unspent.getEntry());	
				}				
			}
		}
		
		//Now check after all that -  valid amounts..
		if(!trans.checkValidInOutPerToken()) {
			//The contract execution log - will be updated later, but added now
			JSONObject errorlog = new JSONObject();
			zContractLog.add(errorlog);
			errorlog.put("error", "Total Inputs are LESS than Total Outputs for certain Tokens");
			return false;
		}
		
		//Add All KNOWN Tokens..
		if(zTouchMMR) {
			if(newtokdets != null) {
				zDB.getUserDB().addTokenDetails(newtokdets);
			}
			
			//Add all the tokens..
			ArrayList<TokenProof> tokens =  zWit.getAllTokenDetails();
			for(TokenProof tp : tokens) {
				zDB.getUserDB().addTokenDetails(tp);
			}
		}
				
		//All OK!
		return true;
	}
}
