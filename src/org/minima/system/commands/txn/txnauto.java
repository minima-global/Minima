package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.ScriptRow;
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
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;
import org.minima.utils.json.JSONObject;

public class txnauto extends Command {

	public txnauto() {
		super("txnauto","[id:] [amount:] [address:] - Create a send transaction automatically");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","amount","address"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		
		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id = getParam("id");
		
		//The details..
		String address 		= getAddressParam("address");
		MiniNumber amount	= getNumberParam("amount");
		
		if(db.getTransactionRow(id) != null) {
			throw new CommandException("Txn with this ID already exists : "+id);
		}
		
		//Create the Txn
		db.createTransaction(id);
		
		//And now add all the relevant coins..
		
		
		
		
		JSONObject ret = getJSONReply();
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnauto();
	}

	public static TxnRow createTransaction(String zAddress, MiniNumber zAmount) throws CommandException {
		
		//The Full Txn..
		TxnRow txnrow = new TxnRow("temp", new Transaction(), new Witness());
		
		//Get the DBs
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
		Wallet walletdb 	= MinimaDB.getDB().getWallet();
		TxPoWTreeNode tip 	= MinimaDB.getDB().getTxPoWTree().getTip();
		
		//How much are we sending.. What are we Burning..
		MiniNumber sendamount 	= zAmount;
		
		//Check valid - for Minima..
		if(!sendamount.isValidMinimaValue()) {
			throw new CommandException("Invalid Minima amount to send.. "+sendamount.toString());
		}
		
		//Lets build a transaction..
		ArrayList<Coin> relcoins = TxPoWSearcher.getRelevantUnspentCoins(tip,"0x00",true);
		
		//The current total
		MiniNumber currentamount 	= MiniNumber.ZERO;
		ArrayList<Coin> currentcoins = new ArrayList<>();
		
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
		
		//Check valid - for Minima..
		if(!sendamount.isValidMinimaValue()) {
			throw new CommandException("Invalid Minima amount to send.. "+sendamount.toString());
		}
		
		//Do we need to send change..
		if(change.isMore(MiniNumber.ZERO)) {
			//Create a new address
			ScriptRow newwalletaddress = MinimaDB.getDB().getWallet().getDefaultAddress();
			MiniData chgaddress = new MiniData(newwalletaddress.getAddress());
			
			//Get the scaled token ammount..
			MiniNumber changeamount = change;
			
			//Change coin does not keep the state
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, changeamount, Token.TOKENID_MINIMA, false);
			
			//And finally.. add the change output
			transaction.addOutput(changecoin);
		}
		
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
		
		return txnrow;
	}
	
}
