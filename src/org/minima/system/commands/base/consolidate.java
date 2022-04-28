package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class consolidate extends Command {

	public consolidate() {
		super("consolidate","[tokenid:] (coinage:) - Consolidate coins by sending them back to yourself");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//The tokenid
		String tokenid = getParam("tokenid");
		
		//How old must the coins
		MiniNumber coinage = getNumberParam("coinage", GlobalParams.MINIMA_CONFIRM_DEPTH);
		
		//Get the DBs
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
		Wallet walletdb 	= MinimaDB.getDB().getWallet();
		TxPoWTreeNode tip 	= MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Current block
		MiniNumber tipblock = tip.getBlockNumber();
		
		//Lets build a transaction..
		ArrayList<Coin> relcoins 	= TxPoWSearcher.getRelevantUnspentCoins(tip,tokenid,true);
		
		//Sort coins via same address - since they require the same signature
		Collections.sort(relcoins, new Comparator<Coin>() {
			@Override
			public int compare(Coin zCoin1, Coin zCoin2) {
				return zCoin1.getAddress().getDataValue().compareTo(zCoin2.getAddress().getDataValue());
			}
		});
		
		int COIN_SIZE = relcoins.size();
		if(COIN_SIZE<2) {
			throw new CommandException("Not enough coins ("+COIN_SIZE+") to consolidate");
		}
		
		int MAX_COINS			= 5;
		TxPoW txpow 			= null;
		
		//Keep building a bigger and bigger transaction..
		int coincounter				= 0;
		MiniNumber currentamount 	= MiniNumber.ZERO;
		Token token 				= null;
		
		while(true) {
			//The current total
			ArrayList<Coin> currentcoins 	= new ArrayList<>();
			currentamount 					= MiniNumber.ZERO;
			token 							= null;
			coincounter						= 0;
			
			//Now cycle through..
			for(Coin coin : relcoins) {
				
				String coinidstr = coin.getCoinID().to0xString();
				
				//Check if we are already using thewm in another Transaction that is being mined
				if(txminer.checkForMiningCoin(coinidstr)) {
					continue;
				}
				
				//Check if in mempool..
				if(txpdb.checkMempoolCoins(coin.getCoinID())) {
					continue;
				}
				
				//Check the Coin Age is enough
				if(tipblock.sub(coin.getBlockCreated()).isLess(coinage)) {
					continue;
				}
				
				//Add this coin..
				currentcoins.add(coin);
				
				//Get the actual ammount..
				currentamount = currentamount.add(coin.getAmount());
				coincounter++;
				
				//Store the token
				if(!tokenid.equals("0x00") && token==null) {
					token = coin.getToken();		
				}
				
				//Do we have enough..
				if(coincounter>=MAX_COINS) {
					break;
				}
			}
			
			//Any coins..
			if(coincounter == 0) {
				throw new CommandException("No coins found of that age..");
			}
			
			//Lets construct a txn..
			Transaction transaction 	= new Transaction();
			Witness witness 			= new Witness();
			
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
				ScriptRow srow 		= walletdb.getScriptFromAddress(scraddress);
				if(srow == null) {
					throw new CommandException("SERIOUS ERROR script missing for simple address : "+scraddress);
				}
				ScriptProof pscr = new ScriptProof(srow.getScript());
				witness.addScript(pscr);
				
				//Add this address to the list we need to sign as..
				String pubkey = srow.getPublicKey();
				if(!reqsigs.contains(pubkey)) {
					reqsigs.add(pubkey);
				}
			}
			
			//Create a new address to collect all the money
			ScriptRow newwalletaddress = MinimaDB.getDB().getWallet().getDefaultAddress();
			MiniData myaddress = new MiniData(newwalletaddress.getAddress());
			
			//Change coin does not keep the state
			Coin returnaddress = new Coin(Coin.COINID_OUTPUT, myaddress, currentamount, new MiniData(tokenid), false);
			if(!tokenid.equals("0x00")) {
				returnaddress.resetTokenID(new MiniData(tokenid));
				returnaddress.setToken(token);
			}
			
			//And finally.. add the change output
			transaction.addOutput(returnaddress);
			
			//Compute the correct CoinID
			TxPoWGenerator.precomputeTransactionCoinID(transaction);
			
			//Calculate the TransactionID..
			transaction.calculateTransactionID();
			
			//Now that we have constructed the transaction - lets sign it..
			for(String pubk : reqsigs) {
	
				//Use the wallet..
				Signature signature = walletdb.signData(pubk, transaction.getTransactionID());
				
				//Add it..
				witness.addSignature(signature);
			}
	
			//The final TxPoW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness);
			
			//Calculate the txpowid / size..
			txpow.calculateTXPOWID();
		
			//How large is the transaction..
			long size = txpow.getSizeinBytes();
			if(size>40000 || MAX_COINS==COIN_SIZE) {
				MinimaLogger.log("Consolidate coins.. txpow size:"+size+" coins:"+coincounter+"/"+COIN_SIZE);
				break;
			}
			
			//Run again
			MinimaLogger.log("TxPoW built with "+coincounter+"/"+COIN_SIZE+" coins, size "+size+".. attempt to add more coins..");
			MAX_COINS++;
		}
		
		JSONObject resp = new JSONObject();
		resp.put("tokenid", tokenid);
		resp.put("allcoins", COIN_SIZE);
		resp.put("consolidated", coincounter);
		
		MiniNumber sendamount = currentamount;
		if(token!=null) {
			sendamount = token.getScaledTokenAmount(currentamount);
		}
		
		resp.put("amount", sendamount.toString());
		resp.put("size", txpow.getSizeinBytes());
		
		//All good..
		ret.put("response", resp);
				
		//Send it to the Miner..
		Main.getInstance().getTxPoWMiner().mineTxPoW(txpow);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new consolidate();
	}
}
