package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;

public class txnutils {

	public static void setMMRandScripts(Transaction zTransaction, Witness zWitness) throws Exception {
		setMMRandScripts(zTransaction, zWitness, true);
	}
	
	public static void setMMRandScripts(Transaction zTransaction, Witness zWitness, boolean zExitOnFail) throws Exception {
		//get the tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Get all the input coins..
		ArrayList<Coin> baseinputs = zTransaction.getAllInputs();
		
		//Are any of the inputs floating
		ArrayList<Coin> inputs = new ArrayList<>();
		for(Coin cc : baseinputs) {
			if(cc.getCoinID().isEqual(Coin.COINID_ELTOO)) {
			
				//Get the MOST recent coin to attach to this transaction..
				Coin floater = TxPoWSearcher.getFloatingCoin(tip, cc.getAmount(), cc.getAddress(), cc.getTokenID());	
				
				if(floater == null) {
					if(zExitOnFail) {
						throw new CommandException("Could not find valid unspent coin for "+cc.toJSON());
					}
				}else {
					inputs.add(floater);
				}
				
			}else {
				
				//Get the complete coin given the CoinID 
				//could be a pre-made coin.. so use correct MMREntry / Created 
				Coin current = TxPoWSearcher.searchCoin(cc.getCoinID());
				if(current == null) {
					if(zExitOnFail) {
						throw new CommandException("Coin with CoinID not found : "+cc.getCoinID().to0xString());
					}
				}else {
					inputs.add(current);
				}
			}
		}
		
		//Min depth of a coin
		MiniNumber minblock = MiniNumber.ZERO;
				
		//Add the inputs..
		for(Coin input : inputs) {
			//How deep
			if(input.getBlockCreated().isMore(minblock)) {
				minblock = input.getBlockCreated();
			}
		}
		
		//Get the block..
		MiniNumber currentblock = tip.getBlockNumber();
		MiniNumber blockdiff 	= currentblock.sub(minblock);
		if(blockdiff.isMore(GlobalParams.MINIMA_MMR_PROOF_HISTORY)) {
			blockdiff = GlobalParams.MINIMA_MMR_PROOF_HISTORY;
		}
		
		//Now get that Block
		TxPoWTreeNode mmrnode = tip.getPastNode(tip.getBlockNumber().sub(blockdiff));
		if(mmrnode == null) {
			//Not enough blocks..
			throw new Exception("Not enough blocks in chain to make valid MMR Proofs..");
		}
		
		//Get the main Wallet
		Wallet walletdb = MinimaDB.getDB().getWallet();
		
		//Add the MMR proofs for the coins..
		for(Coin input : inputs) {
			
			//Get the proof..
			MMRProof proof = mmrnode.getMMR().getProofToPeak(input.getMMREntryNumber());
			
			//Create the CoinProof..
			CoinProof cp = new CoinProof(input, proof);
			
			//Add it to the witness data
			zWitness.addCoinProof(cp);
			
			//Add the script proofs
			String scraddress 	= input.getAddress().to0xString();
			KeyRow keyrow 		= walletdb.getKeysRowFromAddress(scraddress); 
			if(keyrow == null) {
				if(zExitOnFail) {
					throw new Exception("SERIOUS ERROR script missing for simple address : "+scraddress);
				}
			}else {
				ScriptProof pscr = new ScriptProof(keyrow.getScript());
				zWitness.addScript(pscr);
			}
		}
	}
	
	public static void setMMRandScripts(Coin zCoin, Witness zWitness) throws Exception {
		//get the tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Min depth of a coin
		MiniNumber minblock = MiniNumber.ZERO;
		
		//How deep
		if(zCoin.getBlockCreated().isMore(minblock)) {
			minblock = zCoin.getBlockCreated();
		}		
		
		//Get the block..
		MiniNumber currentblock = tip.getBlockNumber();
		MiniNumber blockdiff 	= currentblock.sub(minblock);
		if(blockdiff.isMore(GlobalParams.MINIMA_MMR_PROOF_HISTORY)) {
			blockdiff = GlobalParams.MINIMA_MMR_PROOF_HISTORY;
		}
		
		//Now get that Block
		TxPoWTreeNode mmrnode = tip.getPastNode(tip.getBlockNumber().sub(blockdiff));
		if(mmrnode == null) {
			//Not enough blocks..
			throw new Exception("Not enough blocks in chain to make valid MMR Proofs..");
		}
		
		//Get the main Wallet
		Wallet walletdb = MinimaDB.getDB().getWallet();
			
		//Get the proof..
		MMRProof proof = mmrnode.getMMR().getProofToPeak(zCoin.getMMREntryNumber());
		
		//Create the CoinProof..
		CoinProof cp = new CoinProof(zCoin, proof);
		
		//Add it to the witness data
		zWitness.addCoinProof(cp);
		
		//Add the script proofs
		String scraddress 	= zCoin.getAddress().to0xString();
		KeyRow keyrow 		= walletdb.getKeysRowFromAddress(scraddress); 
		if(keyrow == null) {
			throw new Exception("SERIOUS ERROR script missing for simple address : "+scraddress);
		}
		
		ScriptProof pscr = new ScriptProof(keyrow.getScript());
		zWitness.addScript(pscr);
	}
	
	public static TxnRow createBurnTransaction(ArrayList<String> zExcludeCoins, MiniData zLinkTransactionID, MiniNumber zAmount) throws CommandException {
		
		//The Full Txn..
		TxnRow txnrow = new TxnRow("temp", new Transaction(), new Witness());
		
		//Get the DBs
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
		Wallet walletdb 	= MinimaDB.getDB().getWallet();
		TxPoWTreeNode tip 	= MinimaDB.getDB().getTxPoWTree().getTip();
		
		//How much are we sending.. What are we Burning..
		MiniNumber sendamount 	= zAmount;
		
		//Lets build a transaction..
		ArrayList<Coin> relcoins = TxPoWSearcher.getRelevantUnspentCoins(tip,"0x00",true);
		
		//The current total
		MiniNumber currentamount 	= MiniNumber.ZERO;
		ArrayList<Coin> currentcoins = new ArrayList<>();
		
		//Now cycle through..
		for(Coin coin : relcoins) {
			
			String coinidstr = coin.getCoinID().to0xString();
		
			//Is it to be excluded..
			if(zExcludeCoins.contains(coinidstr)) {
				continue;
			}
			
			//Check if we are already using thewm in another Transaction that is being mined
			if(txminer.checkForMiningCoin(coinidstr)) {
				continue;
			}
			
			//Check if in mempool..
			if(txpdb.checkMempoolCoins(coin.getCoinID())) {
				continue;
			}
			
			//Add this coin..
			currentcoins.add(coin);
			
			//Get the actual ammount..
			currentamount = currentamount.add(coin.getAmount());
			
			//Do we have enough..
			if(currentamount.isMoreEqual(sendamount)) {
				break;
			}
		}
		
		//Did we add enough
		if(currentamount.isLess(sendamount)) {
			//Not enough funds..
			throw new CommandException("Not enough funds / coins for the burn..");
		}
		
		//What is the change..
		MiniNumber change = currentamount.sub(sendamount); 
		
		//Lets construct a txn..
		Transaction transaction 	= txnrow.getTransaction();
		Witness witness 			= txnrow.getWitness();
		
		//Min depth of a coin
		MiniNumber minblock = MiniNumber.ZERO;
				
		//Add the inputs..
		for(Coin inputs : currentcoins) {
			
			//Add this input to our transaction
			transaction.addInput(inputs);
			
			//How deep
			if(inputs.getBlockCreated().isMore(minblock)) {
				minblock = inputs.getBlockCreated();
			}
		}
		
		//Get the block..
		MiniNumber currentblock = tip.getBlockNumber();
		MiniNumber blockdiff 	= currentblock.sub(minblock);
		if(blockdiff.isMore(GlobalParams.MINIMA_MMR_PROOF_HISTORY)) {
			blockdiff = GlobalParams.MINIMA_MMR_PROOF_HISTORY;
		}
		
		//Now get that Block
		TxPoWTreeNode mmrnode = tip.getPastNode(tip.getBlockNumber().sub(blockdiff));
		if(mmrnode == null) {
			//Not enough blocks..
			throw new CommandException("Not enough blocks in chain to make valid MMR Proofs..");
		}
		
		//Create a list of the required signatures
		ArrayList<String> reqsigs = new ArrayList<>();
		
		//Add the MMR proofs for the coins..
		for(Coin input : currentcoins) {
			
			//Get the proof..
			MMRProof proof = mmrnode.getMMR().getProofToPeak(input.getMMREntryNumber());
			
			//Create the CoinProof..
			CoinProof cp = new CoinProof(input, proof);
			
			//Add it to the witness data
			witness.addCoinProof(cp);
			
			//Add the script proofs
			String scraddress 	= input.getAddress().to0xString();
			KeyRow keyrow 		= walletdb.getKeysRowFromAddress(scraddress); 
			if(keyrow == null) {
				throw new CommandException("SERIOUS ERROR script missing for simple address : "+scraddress);
			}
			
			ScriptProof pscr = new ScriptProof(keyrow.getScript());
			witness.addScript(pscr);
			
			//Add this address to the list we need to sign as..
			String priv = keyrow.getPrivateKey();
			if(!reqsigs.contains(priv)) {
				reqsigs.add(priv);
			}
		}
		
		//Check valid - for Minima..
		if(!sendamount.isValidMinimaValue()) {
			throw new CommandException("Invalid Minima amount to send.. "+sendamount.toString());
		}
		
		//Do we need to send change..
		if(change.isMore(MiniNumber.ZERO)) {
			//Create a new address
			KeyRow newwalletaddress = MinimaDB.getDB().getWallet().getDefaultKeyAddress();
			MiniData chgaddress = new MiniData(newwalletaddress.getAddress());
			
			//Get the scaled token ammount..
			MiniNumber changeamount = change;
			
			//Change coin does not keep the state
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, changeamount, Token.TOKENID_MINIMA, false);
			
			//And finally.. add the change output
			transaction.addOutput(changecoin);
		}
		
		//Set the Link Hash! - as this is a BURN transaction
		transaction.setLinkHash(zLinkTransactionID);
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(transaction);
		
		//Calculate the TransactionID..
		transaction.calculateTransactionID();
		
		//Now that we have constructed the transaction - lets sign it..
		for(String priv : reqsigs) {

			//Use the wallet..
			Signature signature = walletdb.sign(priv, transaction.getTransactionID());
			
			//Add it..
			witness.addSignature(signature);
		}
		
		return txnrow;
	}
}
