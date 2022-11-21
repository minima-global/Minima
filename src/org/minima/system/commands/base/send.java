package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
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
import org.minima.system.commands.txn.txnutils;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class send extends Command {

	public class AddressAmount {
		
		MiniData 	mAddress;
		MiniNumber 	mAmount;
		
		public AddressAmount(MiniData zAddress, MiniNumber zAmount) {
			mAddress 	= zAddress;
			mAmount		= zAmount;
		}
		
		public MiniData getAddress(){
			return mAddress;
		}
		
		public MiniNumber getAmount() {
			return mAmount;
		}
	}
	
	public send() {
		super("send","(address:Mx..|0x..) (amount:) (multi:[address:amount,..]) (tokenid:) (state:{}) (burn:) (split:) (mine:) (debug:) (dryrun:) - Send Minima or Tokens to an address");
	}
	
	@Override
	public String getFullHelp() {
		return "\nsend\n"
				+ "\n"
				+ "Send Minima or custom tokens to a wallet or custom script address.\n"
				+ "\n"
				+ "Optionally, send to multiple addresses in one transaction; split UTxOs; add state variables or include a burn.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    A Minima 0x or Mx wallet address or custom script address. Must also specify amount.\n"
				+ "\n"
				+ "amount: (optional)\n"
				+ "    The amount of Minima or custom tokens to send to the specified address.\n"
				+ "\n"
				+ "multi: (optional)\n"
				+ "    JSON Array listing addresses and amounts to send in one transaction.\n"
				+ "    Takes the format [address:amount,address2:amount2,..], with each set in double quotes.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    If sending a custom token, you must specify its tokenid. Defaults to Minima (0x00).\n"
				+ "\n"
				+ "state: (optional)\n"
				+ "    List of state variables, if sending coins to a script. A JSON object in the format {\"port\":\"value\",..}\n"
				+ "\n"
				+ "burn: (optional)\n"
				+ "    The amount of Minima to burn with this transaction.\n"
				+ "\n"
				+ "split: (optional)\n"
				+ "    You can set the number of coins the recipient will receive, between 1 and 20. Default is 1.\n"
				+ "    The amount being sent will be split into multiple coins of equal value.\n"
				+ "    You can split your own coins by sending to your own address.\n"
				+ "    Useful if you want to send multiple transactions without waiting for change to be confirmed.\n"
				+ "\n"
				+ "debug: (optional)\n"
				+ "    true or false, true will print more detailed logs.\n"
				+ "\n"
				+ "dryrun: (optional)\n"
				+ "    true or false, true will simulate the send transaction but not execute it.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "send address:0xFF.. amount:10\n"
				+ "\n"
				+ "send address:0xFF.. amount:10 tokenid:0xFED5.. burn:0.1\n"
				+ "\n"
				+ "send address:0xFF.. amount:10 split:5 burn:0.1\n"
				+ "\n"
				+ "send multi:[\"0xFF..:10\",\"0xEE..:10\",\"0xDD..:10\"] split:20\n"
				+ "\n"
				+ "send amount:1 address:0xFF.. state:{\"0\":\"0xEE..\",\"1\":\"0xDD..\"}\n";
					
	}

	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","uid",
				"address","amount","multi","tokenid","state","burn","split","debug","dryrun","mine"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Who are we sending to
		ArrayList<AddressAmount> recipients = new ArrayList<>();
		
		//What is the total amount we are sending..
		MiniNumber totalamount = MiniNumber.ZERO;
		
		//Is it a MULTI send..
		if(existsParam("multi")) {
			
			//Convert the list..
			JSONArray allrecips = getJSONArrayParam("multi");
			Iterator<String> it = allrecips.iterator(); 
			while(it.hasNext()) {
				String sendto = it.next();
				
				StringTokenizer strtok = new StringTokenizer(sendto,":");
				
				//Get the address
				String address 	= strtok.nextToken();
				MiniData addr 	= null; 
				if(address.toLowerCase().startsWith("mx")) {
					//Convert back to normal hex..
					try {
						addr = Address.convertMinimaAddress(address);
					}catch(IllegalArgumentException exc) {
						throw new CommandException(exc.toString());
					}
				}else {
					addr = new MiniData(address);
				}
				
				//Get the amount
				MiniNumber amount 	= new MiniNumber(strtok.nextToken());
				totalamount 		= totalamount.add(amount);
				
				//Add to our List
				recipients.add(new AddressAmount(addr, amount));
			}
			
		}else {
			//Get the address
			MiniData sendaddress	= new MiniData(getAddressParam("address"));
			
			//How much to send
			MiniNumber sendamount 	= getNumberParam("amount");
			totalamount 			= sendamount;
			
			recipients.add(new AddressAmount(sendaddress, sendamount));
		}
		
		//What is the Token
		String tokenid = getParam("tokenid", "0x00");
		
		//Show extra info..
		boolean debug 	= getBooleanParam("debug", false);
		boolean dryrun 	= getBooleanParam("dryrun", false);
		if(dryrun) {
			debug = true;
		}
		
		//Is there a burn..
		MiniNumber burn  = getNumberParam("burn",MiniNumber.ZERO);
		if(burn.isLess(MiniNumber.ZERO)) {
			throw new CommandException("Cannot have negative burn "+burn.toString());
		}
		
		//Are we splitting the outputs
		MiniNumber split = getNumberParam("split", MiniNumber.ONE);
		if(split.isLess(MiniNumber.ONE) || split.isMore(MiniNumber.TWENTY)) {
			throw new CommandException("Split outputs from 1 to 20");
		}
		
		//Are we doing a Minima burn
		if(tokenid.equals("0x00")) {
			totalamount = totalamount.add(burn);
		}
		
		//Get the State
		JSONObject state = new JSONObject();
		if(existsParam("state")) {
			state = getJSONObjectParam("state");
		}
		
		//Are we Mining synchronously
		boolean minesync = getBooleanParam("mine", false);
		
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
		
		//How old do the coins need to be.. used by consolidate
		MiniNumber coinage = getNumberParam("coinage", MiniNumber.ZERO);
				
		//Lets build a transaction..
		ArrayList<Coin> foundcoins	= TxPoWSearcher.getRelevantUnspentCoins(tip,tokenid,true);
		ArrayList<Coin> relcoins 	= new ArrayList<>();
		
		//Now make sure they are old enough
		MiniNumber mincoinblock = tip.getBlockNumber().sub(coinage);
		for(Coin relc : foundcoins) {
			if(relc.getBlockCreated().isLessEqual(mincoinblock)) {
				relcoins.add(relc);
			}
		}
		
		//Are there any coins at all..
		if(relcoins.size()<1) {
			throw new CommandException("No Coins of tokenid:"+tokenid+" available!");
		}
		
		//Lets select the correct coins..
		MiniNumber findamount = totalamount;
		if(!tokenid.equals("0x00")) {
			findamount 	= relcoins.get(0).getToken().getScaledMinimaAmount(totalamount);
		}
		
		//Now search for the best coin selection.. leave for Now!..
		relcoins = selectCoins(relcoins, findamount, debug);
		
		//The current total
		MiniNumber currentamount 		= MiniNumber.ZERO;
		ArrayList<Coin> currentcoins 	= new ArrayList<>();
		
		if(debug) {
			MinimaLogger.log("Coins that will be checked for transaction");
			for(Coin coin : relcoins) {
				MinimaLogger.log("Coin : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
			}
		}
		
		//Now cycle through..
		Token token = null;
		for(Coin coin : relcoins) {
			
			//Check if we are already using thewm in another Transaction that is being mined
			if(txminer.checkForMiningCoin(coin.getCoinID().to0xString())) {
				if(debug) {
					MinimaLogger.log("Coin being mined : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
				}
				continue;
			}
			
			//Check if in mempool..
			if(txpdb.checkMempoolCoins(coin.getCoinID())) {
				if(debug) {
					MinimaLogger.log("Coin in mempool : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
				}
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
			
			if(debug) {
				MinimaLogger.log("Coin added : "+coin.getAmount()+" "+coin.getCoinID().to0xString()+" total:"+currentamount);
			}
			
			//Do we have enough..
			if(currentamount.isMoreEqual(totalamount)) {
				break;
			}
		}
		
		//Check the token script
		if(token != null) {
			String script = token.getTokenScript().toString();
			if(!script.equals("RETURN TRUE")) {
				//Not enough funds..
				ret.put("status", false);
				ret.put("message", "Token script is not simple : "+script);
				return ret;
			}
		}
		
		//Did we add enough
		if(currentamount.isLess(totalamount)) {
			//Not enough funds..
			ret.put("status", false);
			ret.put("message", "Insufficient funds.. you only have "+currentamount+" require:"+totalamount);
			return ret;
		}
		
		if(debug) {
			MinimaLogger.log("Total Coins used : "+currentcoins.size());
		}
		
		//What is the change..
		MiniNumber change = currentamount.sub(totalamount); 
		
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
			
			//May need it for BURN
			addedcoinid.add(input.getCoinID().to0xString());
			
			//Get the proof..
			MMRProof proof = mmrnode.getMMR().getProofToPeak(input.getMMREntryNumber());
			
			//Create the CoinProof..
			CoinProof cp = new CoinProof(input, proof);
			
			//Add it to the witness data
			witness.addCoinProof(cp);
			
			//Add the script proofs
			String scraddress 	= input.getAddress().to0xString();
			
			//Get the ScriptRow..
			ScriptRow srow = walletdb.getScriptFromAddress(scraddress);
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
		
		//Now make the sendamount correct
		if(!tokenid.equals("0x00")) {
			
			//Convert back and forward to make sure is a valid amount
			MiniNumber tokenamount 	= token.getScaledMinimaAmount(totalamount); 
			MiniNumber prectest 	= token.getScaledTokenAmount(tokenamount);
			
			if(!prectest.isEqual(totalamount)) {
				throw new CommandException("Invalid Token amount to send.. "+totalamount);
			}
			
			totalamount = tokenamount;
					
		}else {
			//Check valid - for Minima..
			if(!totalamount.isValidMinimaValue()) {
				throw new CommandException("Invalid Minima amount to send.. "+totalamount);
			}
		}
		
		//Are we splitting the outputs
		int isplit = split.getAsInt();
		
		//Cycle through all the recipients
		for(AddressAmount user : recipients) {
			
			MiniNumber splitamount 	= user.getAmount().div(split);
			MiniData address 		= user.getAddress();
			
			if(!tokenid.equals("0x00")) {
				//Use the token object we previously found
				splitamount = token.getScaledMinimaAmount(splitamount);
			}
			
			for(int i=0;i<isplit;i++) {
				//Create the output
				Coin recipient = new Coin(Coin.COINID_OUTPUT, address, splitamount, Token.TOKENID_MINIMA, true);
				
				//Do we need to add the Token..
				if(!tokenid.equals("0x00")) {
					recipient.resetTokenID(new MiniData(tokenid));
					recipient.setToken(token);
				}
				
				//Add to the Transaction
				transaction.addOutput(recipient);
			}
		}
		
		//Do we need to send change..
		if(debug) {
			MinimaLogger.log("Change amount : "+change);
		}
		
		if(change.isMore(MiniNumber.ZERO)) {
			//Create a new address
			ScriptRow newwalletaddress = MinimaDB.getDB().getWallet().getDefaultAddress();
			MiniData chgaddress = new MiniData(newwalletaddress.getAddress());
			
			//Get the scaled token ammount..
			MiniNumber changeamount = change;
			if(!tokenid.equals("0x00")) {
				//Use the token object we previously found
				changeamount = token.getScaledMinimaAmount(change);
			}
			
			//Change coin does not keep the state
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, changeamount, Token.TOKENID_MINIMA, false);
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
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(transaction);
		
		//Calculate the TransactionID..
		transaction.calculateTransactionID();
		
		//Now that we have constructed the transaction - lets sign it..
		if(debug) {
			MinimaLogger.log("Total signatures required : "+reqsigs.size());
		}
		
		for(String pubkey : reqsigs) {
			if(debug) {
				MinimaLogger.log("Signing transction with : "+pubkey);
			}
			
			if(!dryrun) {
				//Use the wallet..
				Signature signature = walletdb.signData(pubkey, transaction.getTransactionID());
				
				//Add it..
				witness.addSignature(signature);
			}else {
				MinimaLogger.log("DRY RUN - not signing");
			}
		}
		
		//The final TxPoW
		TxPoW txpow = null;
		
		//Is there a BURN..
		if(!tokenid.equals("0x00") && burn.isMore(MiniNumber.ZERO)) {
			
			//Create a Burn Transaction
			TxnRow burntxn = txnutils.createBurnTransaction(addedcoinid,transaction.getTransactionID(),burn);

			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness, burntxn.getTransaction(), burntxn.getWitness());
		
		}else {
			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness);
		}
		
		//Calculate the txpowid / size..
		txpow.calculateTXPOWID();
		
		if(!dryrun) {
			
			//Sync or Asyn mining..
			if(minesync) {
				boolean success = Main.getInstance().getTxPoWMiner().MineMaxTxPoW(false, txpow, 120000);
				
				if(!success) {
					throw new CommandException("FAILED TO MINE txn in 120 seconds !?");
				}
				
			}else {
				Main.getInstance().getTxPoWMiner().mineTxPoWAsync(txpow);
			}
			
		}else {
			MinimaLogger.log("DRY RUN - not sending");
		}
		
		//All good..
		ret.put("dryrun", dryrun);
		ret.put("response", txpow.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new send();
	}

	
	/**
	 * Coin Selection Algorithm..
	 * 
	 * Which coins to use when sending a transaction
	 * Expects all the coins to be of the same tokenid
	 */
	
	public static ArrayList<Coin> selectCoins(ArrayList<Coin> zAllCoins, MiniNumber zAmountRequired){
		return selectCoins(zAllCoins, zAmountRequired, false);
	}
	
	public static ArrayList<Coin> selectCoins(ArrayList<Coin> zAllCoins, MiniNumber zAmountRequired, boolean zDebug){
		ArrayList<Coin> ret = new ArrayList<>();
		
		//Get the TxPoWDB
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
		
		//First sort the coins by size and address..
		ArrayList<Coin> coinlist = orderCoins(zAllCoins);

		//Are we debugging..
		if(zDebug) {
			MinimaLogger.log("All Selection coins");
			for(Coin coin : coinlist) {
				MinimaLogger.log("Coin found : "+coin.getAmount()+" "+coin.getCoinID().to0xString()+" @ "+coin.getAddress().to0xString());
			}
			
			MinimaLogger.log("Now checking coins");
		}
		
		//Now go through and pick a coin big enough.. but keep looking for smaller coins  
		boolean found    = false;
		Coin currentcoin = null;
		for(Coin coin : coinlist) {
			
			//Check if we are already using thewm in another Transaction that is being mined
			if(txminer.checkForMiningCoin(coin.getCoinID().to0xString())) {
				if(zDebug) {
					MinimaLogger.log("Coin being mined : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
				}
				continue;
			}
			
			//Check if in mempool..
			if(txpdb.checkMempoolCoins(coin.getCoinID())) {
				if(zDebug) {
					MinimaLogger.log("Coin in mempool : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
				}
				continue;
			}
			
			if(coin.getAmount().isMoreEqual(zAmountRequired)) {
			
				if(zDebug) {
					MinimaLogger.log("Valid Coin found : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
				}
				found 		= true;
				currentcoin = coin;
			}else {
				
				//Not big enough - all others will be smaller..
				if(zDebug) {
					MinimaLogger.log("Coin too small - no more checking : "+coin.getAmount()+" "+coin.getCoinID().to0xString());
				}
				
				break;
			}
		}
		
		//Did we find one..
		if(found) {
			//Add the single coin to the list
			ret.add(currentcoin);
		
			if(zDebug) {
				MinimaLogger.log("Single coin returned : "+currentcoin.getAmount()+" "+currentcoin.getCoinID().to0xString());
			}
			
		}else {

			//Did not find a single coin that satisfies the amount..
			if(zDebug) {
				MinimaLogger.log("Returning all coins..");
			}
			
			//Return them all..
			return coinlist;
		}
		
		//Return what we have..
		return ret;
	}
	
	/**
	 * Order coins by amount and address
	 */
	public static ArrayList<Coin> orderCoins(ArrayList<Coin> zCoins){
		
		ArrayList<Coin> ret = new ArrayList<>();
		
		//First Sort by amount
		Collections.sort(zCoins, new Comparator<Coin>() {
			@Override
			public int compare(Coin zCoin1, Coin zCoin2) {
				MiniNumber amt1 = zCoin1.getAmount();
				MiniNumber amt2 = zCoin2.getAmount();
				return amt2.compareTo(amt1);
			}
		});
		
		//Now cycle through and get the different addresses
		ArrayList<String> addresses = new ArrayList<>();
		String currentaddress = "";
		for(Coin cc : zCoins) {
			String addr = cc.getAddress().to0xString();
			if(!addresses.contains(addr)) {
				addresses.add(addr);
			}
		}
		
		//And now order by address..
		for(String address : addresses) {
			for(Coin cc : zCoins) {
				String caddress = cc.getAddress().to0xString();
				if(caddress.equals(address)) {
					ret.add(cc);
				}
			}
		}
		
		return ret;
	}
	
	public static void main(String[] zArgs) {
		
		ArrayList<Coin> allcoins = new ArrayList<>();
		allcoins.add(new Coin(new MiniData("0xFF"), MiniNumber.ONE, MiniData.ZERO_TXPOWID));
		allcoins.add(new Coin(new MiniData("0xEE"), MiniNumber.TWO, MiniData.ZERO_TXPOWID));
		allcoins.add(new Coin(new MiniData("0xDD"), MiniNumber.TEN, MiniData.ZERO_TXPOWID));
		allcoins.add(new Coin(new MiniData("0xDD"), MiniNumber.ONE, MiniData.ZERO_TXPOWID));
		allcoins.add(new Coin(new MiniData("0xEE"), new MiniNumber("0.5"), MiniData.ZERO_TXPOWID));
		allcoins.add(new Coin(new MiniData("0xEE"), new MiniNumber("0.2"), MiniData.ZERO_TXPOWID));

		ArrayList<Coin> sortedcoins = orderCoins(allcoins);
				
		for(Coin cc : sortedcoins) {
			System.out.println(cc.toJSON().toString());
		}
	}
}