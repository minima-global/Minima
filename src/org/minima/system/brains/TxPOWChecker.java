package org.minima.system.brains;

import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.miniscript.Contract;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.system.input.functions.gimme50;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;

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
		MiniData32 transhash = Crypto.getInstance().hashObject(trans);
		
		//Now cycle
		Witness wit = zTxPOW.getWitness();
		int len     = wit.getAllPubKeys().size();
		
		for(int i=0;i<len;i++) {
			MiniData pubk = wit.getPublicKey(i);
			MiniData sig  = wit.getSignature(i);
			
			//Check it..
			boolean ok = PubPrivKey.verify(pubk, transhash, sig);
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
		return checkTransactionMMR(zTxPOW.getTransaction(), zTxPOW.getWitness(), zDB, zDB.getTopBlock(), zDB.getMainTree().getChainTip().getMMRSet(), false);
	}
	
//	public static boolean checkTransactionMMR(Transaction zTrans, Witness zWit, MinimaDB zDB, MiniNumber zBlockNumber, MMRSet zMMRSet) {
//		return checkTransactionMMR(zTrans, zWit, zDB, zBlockNumber, zMMRSet, true);
//	}
		
	public static boolean checkTransactionMMR(Transaction zTrans, Witness zWit, MinimaDB zDB, MiniNumber zBlockNumber, MMRSet zMMRSet, boolean zTouchMMR) {
		//Check the input scripts
		ArrayList<Coin> inputs  = zTrans.getAllInputs();
		
		//The Signatures
		String sigs = zWit.getAllPubKeysCSV();
				
		//First Inputs..
		MiniNumber totalin = MiniNumber.ZERO;
		int ins = inputs.size();
		for(int i=0;i<ins;i++) {
			//Get the Input
			Coin input = inputs.get(i);

			//Get the Script..
			String script = zWit.getScript(i);
			
			if(input.getCoinID().isExactlyEqual(gimme50.COINID_INPUT) && input.getAmount().isLessEqual(new MiniNumber("50"))){
				//We good.. TESTNET allows up to 50 printed..
				//..
			}else {
				//Check the Address is the hash of the SCRIPT
				Address scraddr = new Address(script);
				if(!scraddr.getAddressData().isExactlyEqual(input.getAddress())) {
					return false;
				}
				
				//Is it a valid input.. UNSPENT in MMR
				MMRProof proof = zWit.getAllProofs().get(i);
				
				//MUST be a full proof - this done in checkproof..
//				if(proof.getMMRData().isHashOnly()) {
//					return false;
//				}
				
				//Is the proof chain valid
				boolean valid = zMMRSet.checkProof(proof);
				if(!valid) {
					return false;
				}
				
				//Is this input for the correct details..
				if(!proof.checkCoin(input)) {
					return false;
				}
				
				if(zTouchMMR) {
					//Update the MMR with this spent coin..
					MMREntry spent = zMMRSet.updateSpentCoin(proof);
				
					//Do we keep it..
					if(zDB.getUserDB().isAddressRelevant(input.getAddress())) {
						zMMRSet.addKeeper(spent.getEntry());	
					}
				}
				
				//Create the Contract to check..
				Contract cc = new Contract(script,sigs,zTrans,false);
				
				//set the environment
				String address = input.getAddress().toString();
				
				cc.setGlobalVariable("@BLKNUM", new NumberValue(zBlockNumber));
				cc.setGlobalVariable("@INPUT", new NumberValue(i));
				cc.setGlobalVariable("@AMOUNT", new NumberValue(input.getAmount()));
				cc.setGlobalVariable("@ADDRESS", new HEXValue(address));
				cc.setGlobalVariable("@TOKENID", new HEXValue(input.getTokenID()));
				cc.setGlobalVariable("@COINID", new HEXValue(input.getCoinID()));
				cc.setGlobalVariable("@SCRIPT", new ScriptValue(script));
				cc.setGlobalVariable("@TOTIN", new NumberValue(zTrans.getAllInputs().size()));
				cc.setGlobalVariable("@TOTOUT", new NumberValue(zTrans.getAllOutputs().size()));
				cc.setGlobalVariable("@INBLKNUM", new NumberValue(proof.getMMRData().getInBlock()));
				
				//Set the Prev State
				cc.setPrevState(proof.getMMRData().getPrevState());
				
				//Run it!
				cc.run();
				
				//and.. ?
				if(!cc.isSuccess()) {
					return false;
				}
			}
			
			//Add to the total
			totalin = totalin.add(input.getAmount());
		}
		
		//The HASH of the Transaction.. needed for coinid
		MiniData32 transhash = Crypto.getInstance().hashObject(zTrans);
				
		//Get outputs - add them to the MMR also..
		MiniNumber totalout = MiniNumber.ZERO;
		ArrayList<Coin> outputs  = zTrans.getAllOutputs();
		int outs = outputs.size();
		for(int i=0;i<outs;i++) {
			//Get the coin..
			Coin output = outputs.get(i);
			
			//Now calculate the CoinID / TokenID
			MiniData32 coinid = Crypto.getInstance().hashObjects(transhash, new MiniByte(i));
			
			//Is this a token create output..
			MiniData32 tokid = output.getTokenID();
			
			if(output.getTokenID().isLessEqual(Coin.TOKENID_CREATE)) {
				//It's a token..
				BigInteger big = output.getTokenID().getDataVaue();
				
				int val = big.intValue();
				
			}
				
			
			
			if(output.getTokenID().isNumericallyEqual(Coin.TOKENID_CREATE)) {
				//Set the TokenID to the CoinID..
				tokid = coinid;
				
				//Make it the HASH ( CoinID | Total Amount )
			}
			
			if(zTouchMMR) {
				//Create a new Coin..
				Coin mmrcoin = new Coin(coinid, output.getAddress(), output.getAmount(), tokid);
				
				//Now add as an unspent to the MMR
				MMRData mmrdata = new MMRData(MiniByte.FALSE, mmrcoin, zBlockNumber, zTrans.getCompleteState());
				
				//And Add it..
				MMREntry unspent = zMMRSet.addUnspentCoin(mmrdata);
				
				//Do we keep it..
				if(zDB.getUserDB().isAddressRelevant(output.getAddress())) {
					//Keep this MMR record
					zMMRSet.addKeeper(unspent.getEntry());	
					
					//Keep the token generation numbers
				}
			}
			
			//Check the total..
			totalout = totalout.add(outputs.get(i).getAmount());
		}
		
		//And final check..
		//TODO.. check tokens as well as base Minima
		if(totalout.isMore(totalin)) {
			return false;
		}
		
		//All OK!
		return true;
	}
}
