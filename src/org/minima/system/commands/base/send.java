package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.StateVariable;
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
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class send extends Command {

	
	public send() {
		super("send","[address:] [amount:] (floating:) (tokenid:) (state:{}) - Send Minima or Tokens to an address");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the details
		String address = (String)getParams().get("address");
		String amount  = (String)getParams().get("amount");
		
		if(address==null || amount==null) {
			throw new CommandException("MUST specify adress and amount");
		}
		
		//Is the coin floating 
		boolean floating = getBooleanParam("floating",false);
		
		JSONObject state = new JSONObject();
		if(existsParam("state")) {
			state = getJSONObjectParam("state");
		}
		
		//How much are we sending..
		MiniNumber sendamount 	= new MiniNumber(amount);
		MiniData sendaddress	= new MiniData(address);
		
		//What is the Token
		String tokenid = "0x00";
		if(getParams().containsKey("tokenid")) {
			tokenid = (String)getParams().get("tokenid");
		}
		
		//get the tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Get the parent deep enough for valid confirmed coins
		int confdepth = GlobalParams.MINIMA_CONFIRM_DEPTH.getAsInt();
		for(int i=0;i<confdepth;i++) {
			tip = tip.getParent();
			if(tip == null) {
				//Insufficient blocks
				ret.put("status", false);
				ret.put("message", "Insufficient blocks..");
				return ret;
			}
		}
		
		//Get the TxPoWDB
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
		
		//Lets build a transaction..
		ArrayList<Coin> relcoins = TxPoWSearcher.getRelevantUnspentCoins(tip,tokenid,true);
		
		//The current total
		MiniNumber currentamount 	= MiniNumber.ZERO;
		ArrayList<Coin> currentcoins = new ArrayList<>();
		
		//Now cycle through..
		Token token = null;
		for(Coin coin : relcoins) {
			
			//Check if we are already using thewm in another Transaction that is being mined
			if(txminer.checkForMiningCoin(coin.getCoinID().to0xString())) {
				continue;
			}
			
			//Check if in mempool..
			if(txpdb.checkMempoolCoins(coin.getCoinID())) {
				continue;
			}
		
			//Add this coin..
			currentcoins.add(coin);
			
			//Get the actual ammount..
			if(tokenid.equals("0x00")) {
				currentamount = currentamount.add(coin.getAmount());
			}else {
				//Store it..
				if(token == null) {
					token = coin.getToken();
				}
				
				//Calculate the Token Amount..
				MiniNumber amt = coin.getToken().getScaledTokenAmount(coin.getAmount());
				
				//Add that to the total
				currentamount = currentamount.add(amt);
			}
			
			//Do we have enough..
			if(currentamount.isMoreEqual(sendamount)) {
				break;
			}
		
		}
		
		//Did we add enough
		if(currentamount.isLess(sendamount)) {
			//Not enough funds..
			ret.put("status", false);
			ret.put("message", "Insufficient funds.. you only have "+currentamount);
			return ret;
		}
		
		//What is the change..
		MiniNumber change = currentamount.sub(sendamount); 
		
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
		
		//Get the main Wallet
		Wallet walletdb = MinimaDB.getDB().getWallet();
		
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
		
		//Now make the sendamount correct
		if(!tokenid.equals("0x00")) {
			
			//Convert back and forward to make sure is a valid amount
			MiniNumber tokenamount 	= token.getScaledMinimaAmount(sendamount); 
			MiniNumber prectest 	= token.getScaledTokenAmount(tokenamount);
			
			if(!prectest.isEqual(sendamount)) {
				throw new CommandException("Invalid Token amount to send.. "+amount);
			}
			
			sendamount = tokenamount;
					
		}else {
			//Check valid - for Minima..
			if(!sendamount.isValidMinimaValue()) {
				throw new CommandException("Invalid Minima amount to send.. "+amount);
			}
		}
		
		//Create the output
		Coin recipient = new Coin(Coin.COINID_OUTPUT, sendaddress, sendamount, Token.TOKENID_MINIMA, floating, true);
		
		//Do we need to add the Token..
		if(!tokenid.equals("0x00")) {
			recipient.resetTokenID(new MiniData(tokenid));
			recipient.setToken(token);
		}
		
		//Add to the Transaction
		transaction.addOutput(recipient);
		
		//Do we need to send change..
		if(change.isMore(MiniNumber.ZERO)) {
			//Create a new address
			KeyRow newwalletaddress = MinimaDB.getDB().getWallet().createNewKey();
			MiniData chgaddress = new MiniData(newwalletaddress.getAddress());
			
			//Get the scaled token ammount..
			MiniNumber changeamount = change;
			if(!tokenid.equals("0x00")) {
				//Use the token object we previously found
				changeamount = token.getScaledMinimaAmount(change);
			}
			
			//Change coin does not keep the state
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, changeamount, Token.TOKENID_MINIMA,false,false);
			if(!tokenid.equals("0x00")) {
				changecoin.resetTokenID(new MiniData(tokenid));
				changecoin.setToken(token);
			}
			
			//And finally.. add the change output
			transaction.addOutput(changecoin);
		}
		
		//Are there any State Variables
		for(Object key : state.keySet()) {
			
			//The Key is a String
			String portstr = (String)key; 
			
			//The port
			int port = Integer.parseInt(portstr);
			
			//Get the state var..
			String var = (String) state.get(key);

			//Create a state variable..
			StateVariable sv = new StateVariable(port, var);
			
			//Add to the transaction..
			transaction.addStateVariable(sv);
		}
		
		//Calculate the TransactionID..
		MiniData transid = Crypto.getInstance().hashObject(transaction);
		
		//Now that we have constructed the transaction - lets sign it..
		for(String priv : reqsigs) {

			//Use the wallet..
			Signature signature = walletdb.sign(priv, transid);
			
			//Add it..
			witness.addSignature(signature);
		}
		
		//Now create a complete TxPOW
		TxPoW txpow = TxPoWGenerator.generateTxPoW(transaction, witness);
		
		//Calculate the size..
		txpow.calculateTXPOWID();
		
		//All good..
		ret.put("response", txpow.toJSON());
				
		//Send it to the Miner..
		Main.getInstance().getTxPoWMiner().mineTxPoW(txpow);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new send();
	}

}
