package org.minima.system.commands.all;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.KeyRow;
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
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.Signature;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.params.GlobalParams;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class tokencreate extends Command {

	public tokencreate() {
		super("tokencreate","[name:] [amount:] (description:) (decimals:) (script:)  - Create a token");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Required
		String name 	= (String)getParams().get("name");
		String amount   = (String)getParams().get("amount");
		
		//Optional
		String description = "";
		if(getParams().containsKey("description")) {
			description	= (String)getParams().get("description");
		}
		
		int decimals = 8;
		if(getParams().containsKey("decimals")) {
			decimals	= Integer.parseInt((String)getParams().get("decimals"));
		}
		
		String script = "RETURN TRUE";
		if(getParams().containsKey("script")) {
			script	= (String)getParams().get("script");
		}
		
		//Now construct the txn..
		if(name==null || amount==null) {
			throw new Exception("MUST specify name and amount");
		}
		
		//The actual amount of tokens..
		MiniNumber totaltoks = new MiniNumber(amount).floor(); 
		MiniNumber totaldecs = MiniNumber.TEN.pow(decimals); 
		
		//How much Minima will it take to colour.. 
		MiniNumber colorminima = MiniNumber.MINI_UNIT.mult(totaldecs).mult(totaltoks);
		
		//What is the scale..
		int scale = MiniNumber.MAX_DECIMAL_PLACES - decimals;
		
				
		//Lets create the token..
		Token createtoken = new Token(Coin.COINID_OUTPUT, 
										new MiniNumber(scale), 
										colorminima,
										new MiniString(name),
										new MiniString(script));
		
		//The actual amount of Minima that needs to be sent
		MiniNumber sendamount = new MiniNumber(colorminima);
		
		//Send it to ourselves
		KeyRow sendkey 			= MinimaDB.getDB().getWallet().createNewKey();
		MiniData sendaddress 	= new MiniData(sendkey.getAddress());
		
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
		
		//Lets build a transaction..
		ArrayList<Coin> relcoins = TxPoWSearcher.getRelevantUnspentCoins(tip);
		
		//The current total
		MiniNumber currentamount 	= MiniNumber.ZERO;
		ArrayList<Coin> currentcoins = new ArrayList<>();
		
		//Now cycle through..
		for(Coin coin : relcoins) {
			
			//Add this coin..
			currentcoins.add(coin);
			currentamount = currentamount.add(coin.getAmount());
			
			//Do we have enough..
			if(currentamount.isMoreEqual(sendamount)) {
				break;
			}
		}
		
		//Did we add enough
		if(currentamount.isLess(sendamount)) {
			//Not enough funds..
			throw new Exception("Insufficient funds.. you only have "+currentamount);
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
		MiniNumber currentblock = tip.getBlockNUmber();
		MiniNumber blockdiff 	= currentblock.sub(minblock);
		if(blockdiff.isMore(GlobalParams.MINIMA_MMR_PROOF_HISTORY)) {
			blockdiff = GlobalParams.MINIMA_MMR_PROOF_HISTORY;
		}
		
		//Now get that Block
		TxPoWTreeNode mmrnode = tip.getPastNode(tip.getBlockNUmber().sub(blockdiff));
		if(mmrnode == null) {
			//Not enough blocks..
			throw new Exception("Not enough blocks in chain to make valid MMR Proofs..");
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
			KeyRow keyrow 		= walletdb.getKeysRow(scraddress); 
			if(keyrow == null) {
				throw new Exception("SERIOUS ERROR script missing for simple address : "+scraddress);
			}
			
			ScriptProof pscr = new ScriptProof(keyrow.getScript());
			witness.addScript(pscr);
			
			//Add this address to the list we need to sign as..
			String priv = keyrow.getPrivateKey();
			if(!reqsigs.contains(priv)) {
				reqsigs.add(priv);
			}
		}
		
		//Now add the output..
		Coin recipient = new Coin(sendaddress, sendamount, Token.TOKENID_CREATE);
		
		//Set the Create Token Details..
		recipient.setToken(createtoken);
		
		//Add to the transaction..
		transaction.addOutput(recipient);
		
		//Do we need to send change..
		if(change.isMore(MiniNumber.ZERO)) {
			//Create a new address
			KeyRow newwalletaddress = MinimaDB.getDB().getWallet().createNewKey();
			MiniData chgaddress 	= new MiniData(newwalletaddress.getAddress());
			
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, change, Token.TOKENID_MINIMA);
			transaction.addOutput(changecoin);
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
		ret.put("response", txpow.getTransaction().toJSON());
				
		//Send it to the Miner..
		Main.getInstance().getTxPoWMiner().mineTxPoW(txpow);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new tokencreate();
	}

}
