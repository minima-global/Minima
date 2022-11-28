package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.ScriptRow;
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
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.Signature;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.txn.txnutils;
import org.minima.system.params.GlobalParams;
import org.minima.utils.json.JSONObject;

public class tokencreate extends Command {

	public tokencreate() {
		super("tokencreate","[name:] [amount:] (decimals:) (script:) (state:{}) (signtoken:) (webvalidate:) (burn:) - Create a token. 'name' can be a JSON Object");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntokencreate\n"
				+ "\n"
				+ "Create (mint) custom tokens or NFTs.\n"
				+ "\n"
				+ "You must have some sendable Minima in your wallet as tokens are 'colored coins', a fraction of 1 Minima.\n"
				+ "\n"
				+ "name:\n"
				+ "    The name of the token. Can be a string or JSON Object.\n"
				+ "\n"
				+ "amount: \n"
				+ "    The amount of total supply to create for the token. Between 1 and 1 Trillion.\n"
				+ "\n"
				+ "decimals: (optional)\n"
				+ "    The number of decimal places for the token. Default is 8, maximum 16.\n"
				+ "    To create NFTs, use 0.\n"
				+ "\n"
				+ "script: (optional)\n"
				+ "    Add a custom script that must return 'TRUE' when spending any coin of this token.\n"
				+ "    Both the token script and coin script must return 'TRUE' for a coin to be sendable.\n"
				+ "\n"
				+ "state: (optional)\n"
				+ "    List of state variables, if adding a script. A JSON object in the format {\"port\":\"value\",..}\n"
				+ "\n"
				+ "signtoken: (optional)\n"
				+ "    Provide a public key to sign the token with.\n"
				+ "    Useful for proving you are the creator of the token/NFT.\n"
				+ "\n"
				+ "webvalidate: (optional)\n"
				+ "    Provide a URL to a publicly viewable .txt file you are hosting which stores the tokenid for validation purposes.\n"
				+ "    Create the file in advance and get the tokenid after the token has been minted.\n"
				+ "\n"
				+ "burn: (optional)\n"
				+ "    Amount to burn with the tokencreate minting transaction.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "tokencreate name:newtoken amount:1000000\n"
				+ "\n"
				+ "tokencreate amount:10 name:{\"name\":\"newcoin\",\"link\":\"http:mysite.com\",\"description\":\"A very cool token\"}\n"
				+ "\n"
				+ "tokencreate name:mynft amount:10 decimals:0 webvalidate:https://www.mysite.com/nftvalidation.txt signtoken:0xFF.. burn:0.1\n"
				+ "\n"
				+ "tokencreate name:charitycoin amount:1000 script:\"ASSERT VERIFYOUT(@TOTOUT-1 0xMyAddress 1 0x00 TRUE)\"\n";				
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"name","amount","decimals","script",
				"state","signtoken","webvalidate","burn"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//Check the basics..
		if(!existsParam("name") || !existsParam("amount")) {
			throw new CommandException("MUST specify name and amount");
		}
		
		//Is there a state JSON
		JSONObject state = new JSONObject();
		if(existsParam("state")) {
			state = getJSONObjectParam("state");
		}
		
		//Is name a JSON
		JSONObject jsonname = null;
		if(isParamJSONObject("name")) {
			
			//Get the JSON
			jsonname = getJSONObjectParam("name");
			
			//make sure there is a name object
			if(!jsonname.containsKey("name")) {
				throw new CommandException("MUST specify a 'name' for the token in the JSON");
			}
			
		}else {
			
			//It's a String.. create a JSON
			jsonname = new JSONObject();
			jsonname.put("name", getParam("name"));
		}
		
		//The amount is always a MiniNumber
		String amount   = (String)getParams().get("amount");
		
		//The burn
		MiniNumber burn = getNumberParam("burn", MiniNumber.ZERO);
		
		//How many decimals - can be 0.. for an NFT
		int decimals = 8;
		if(getParams().containsKey("decimals")) {
			decimals = Integer.parseInt((String)getParams().get("decimals"));
			
			//Safety check.. not consensus set - could be more.
			if(decimals>16) {
				throw new Exception("MAX 16 decimal places");
			}
		}
		
		String script = "RETURN TRUE";
		if(getParams().containsKey("script")) {
			script	= (String)getParams().get("script");
		}
		
		//Now construct the txn..
		if(jsonname==null || amount==null) {
			throw new CommandException("MUST specify name and amount");
		}
		
		//The actual amount of tokens..
		MiniNumber totaltoks = new MiniNumber(amount).floor(); 
		
		//Safety check Amount is within tolerant levels.. could use ALL their Minima otherwise..
		if(totaltoks.isMore(MiniNumber.TRILLION)) {
			throw new CommandException("MAX 1 Trillion coins for a token");
		}else if(totaltoks.isLessEqual(MiniNumber.ZERO)) {
			throw new CommandException("Cannot create less than 1 token");
		}
		
		//Decimals as a number
		MiniNumber totaldecs = MiniNumber.TEN.pow(decimals); 
		
		//How much Minima will it take to colour.. 
		MiniNumber colorminima = MiniNumber.MINI_UNIT.mult(totaldecs).mult(totaltoks);
		
		//What is the scale..
		int scale = MiniNumber.MAX_DECIMAL_PLACES - decimals;
		
		//The actual amount of Minima that needs to be sent
		MiniNumber sendamount 	= new MiniNumber(colorminima);
		
		//Send it to ourselves
		ScriptRow sendkey 		= MinimaDB.getDB().getWallet().getDefaultAddress();
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
		
		//Lets build a transaction.. MUST use Minima to create a token!
		ArrayList<Coin> relcoins = TxPoWSearcher.getRelevantUnspentCoins(tip,"0x00",true);
		
		//The current total
		MiniNumber currentamount 	= MiniNumber.ZERO;
		ArrayList<Coin> currentcoins = new ArrayList<>();
		
		//Get the TxPoWDB
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
		
		//Now cycle through..
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
			currentamount = currentamount.add(coin.getAmount());
			
			//Do we have enough..
			if(currentamount.isMoreEqual(sendamount)) {
				break;
			}
		}
		
		//Did we add enough
		if(currentamount.isLess(sendamount)) {
			//Not enough funds..
			//Insufficient blocks
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
		
		//Which Coins are added
		ArrayList<String> addedcoinid = new ArrayList<>();
				
		//Add the MMR proofs for the coins..
		for(Coin input : currentcoins) {
			
			//Keep for burn calc
			addedcoinid.add(input.getCoinID().to0xString());
			
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
			
			//Add this address / public key to the list we need to sign as..
			String pubkey = srow.getPublicKey();
			if(!reqsigs.contains(pubkey)) {
				reqsigs.add(pubkey);
			}
		}
		
		//Now add the output..
		Coin recipient = new Coin(Coin.COINID_OUTPUT, sendaddress, sendamount, Token.TOKENID_CREATE, true);
		
		//Is there a Web Validation URL
		if(existsParam("webvalidate")) {
			
			//Add to the description
			jsonname.put("webvalidate", getParam("webvalidate"));
		}
		
		//Are we signing the token..
		if(existsParam("signtoken")) {
		
			//What is the coinid of the first input..
			MiniData firstcoinid = transaction.getAllInputs().get(0).getCoinID();
			
			//Calculate the CoinID.. It's the first output
			MiniData tokencoinid = transaction.calculateCoinID(firstcoinid, 0);
			
			//Get the Public Key
			String sigpubkey = getParam("signtoken");
			
			//Now sign the coinid..
			Signature sig = walletdb.signData(sigpubkey, tokencoinid);
			
			//Get the MiniData version..
			MiniData sigdata = MiniData.getMiniDataVersion(sig);
			
			//Get the Pubkey.. add it to the JSON
			jsonname.put("signedby", sigpubkey);
			jsonname.put("signature", sigdata.to0xString());
		}
		
		//Let's create the token..
		Token createtoken = new Token(Coin.COINID_OUTPUT, 
										new MiniNumber(scale), 
										colorminima,
										new MiniString(jsonname.toString()),
										new MiniString(script));
		
		//Set the Create Token Details..
		recipient.setToken(createtoken);
		
		//Add to the transaction..
		transaction.addOutput(recipient);
		
		//Do we need to send change..
		if(change.isMore(MiniNumber.ZERO)) {
			//Create a new address
			ScriptRow newwalletaddress = MinimaDB.getDB().getWallet().getDefaultAddress();
			MiniData chgaddress = new MiniData(newwalletaddress.getAddress());
			
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, change, Token.TOKENID_MINIMA, false);
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
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(transaction);
		
		//Calculate the TransactionID..
		transaction.calculateTransactionID();
		
		//Now that we have constructed the transaction - lets sign it..
		for(String pubkey : reqsigs) {

			//Use the wallet..
			Signature signature = walletdb.signData(pubkey, transaction.getTransactionID());
			
			//Add it..
			witness.addSignature(signature);
		}
		
		//The final TxPoW
		TxPoW txpow = null;
		
		//Is there a BURN..
		if(burn.isMore(MiniNumber.ZERO)) {
			
			//Create a Burn Transaction
			TxnRow burntxn = txnutils.createBurnTransaction(addedcoinid,transaction.getTransactionID(),burn);

			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness, burntxn.getTransaction(), burntxn.getWitness());
		
		}else {
			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness);
		}
		
		//Calculate the size..
		txpow.calculateTXPOWID();
		
		//All good..
		ret.put("response", txpow.getTransaction().toJSON());
				
		//Send it to the Miner..
		Main.getInstance().getTxPoWMiner().mineTxPoWAsync(txpow);
	
		return ret;
	}

	@Override
	public Command getFunction() {
		return new tokencreate();
	}

}
