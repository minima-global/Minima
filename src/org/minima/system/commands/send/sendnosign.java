package org.minima.system.commands.send;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.base.send;
import org.minima.system.commands.txn.txnutils;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class sendnosign extends Command {

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
	
	public sendnosign() {
		super("sendnosign","(address:Mx..|0x..) (amount:) (multi:[address:amount,..]) (tokenid:) (state:{}) (burn:) (split:) (debug:) - Create a txn but don't sign it");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"address","amount","multi",
				"tokenid","state","burn","split","debug","dryrun"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Who are we sending to
		ArrayList<AddressAmount> recipients = new ArrayList<>();
		
		//What is the toal amount we are sending..
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
		relcoins = send.selectCoins(relcoins, findamount, debug);
		
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
		
		//The final TxPoW
		TxPoW txpow = null;
		
		//Is there a BURN..
		if(!tokenid.equals("0x00") && burn.isMore(MiniNumber.ZERO)) {
			
			//Create a Burn Transaction - but NO Signatures
			TxnRow burntxn = txnutils.createBurnTransaction(addedcoinid,transaction.getTransactionID(),burn, false);

			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness, burntxn.getTransaction(), burntxn.getWitness());
		
		}else {
			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(transaction, witness);
		}
		
		//Create the file..
		File txnfile = MiniFile.createBaseFile("unsignedtransaction-"+System.currentTimeMillis()+".txn");
				
		//Write it to a file..
		MiniFile.writeObjectToFile(txnfile, txpow);
				
		JSONObject resp = new JSONObject();
		resp.put("txpow", txnfile.getAbsolutePath());
		
		//All good..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new sendnosign();
	}
}