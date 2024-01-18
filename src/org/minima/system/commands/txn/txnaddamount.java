package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.send.send;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txnaddamount extends Command {

	public txnaddamount() {
		super("txnaddamount","[id:] [amount:] (address) (onlychange:) (tokenid:) - Add inputs and calculate change for a certain amount");
	}
	
	@Override
	public String getFullHelp() {
		return "\txnaddamount\n"
				+ "\n"
				+ "Add a certain amount to a transaction.\n"
				+ "\n"
				+ "Output amount to an address OR only the change - if you have already added an output\n"
				+ "\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","amount","address","onlychange","tokenid","fromaddress"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 			= getParam("id");
		MiniNumber amount	= getNumberParam("amount");
		
		//Could be a token..
		MiniData tokenid	= Token.TOKENID_MINIMA;
		Token token 		= null;
		if(existsParam("tokenid")) {
			tokenid	= getDataParam("tokenid");
			
			//Is it Minima..
			if(!tokenid.isEqual(Token.TOKENID_MINIMA)) {
				token	= TxPoWSearcher.getToken(tokenid);
				if(token == null) {
					throw new CommandException("Token not found : "+tokenid);
				}
			}
		}
		
		//The actual amount
		MiniNumber miniamount = amount;
		if(token != null) {
			miniamount = token.getScaledMinimaAmount(amount);
		}
		
		//Get the Transaction
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		Transaction trans = txnrow.getTransaction();
		
		//Now get coins to the value required..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();

		//Do we even have a tip
		if(tip == null) {
			ret.put("response", new JSONArray());
			return ret;
		}
		
		//Convert to token amount..
		MiniNumber tokenamount = amount;
		if(!tokenid.isEqual(Token.TOKENID_MINIMA)) {
			tokenamount = token.getScaledMinimaAmount(amount);
		}
		
		//Add the amount
		boolean addonlychange = getBooleanParam("onlychange", false);
		
		//Use only one address
		boolean 	useaddress 	= false;
		MiniData 	fromaddress = new MiniData("0x00");
		if(existsParam("fromaddress")) {
			useaddress 	= true;
			fromaddress = new MiniData(getAddressParam("fromaddress"));
		}
		
		//Get all valid coins
		ArrayList<Coin> coins = null;
		if(!useaddress) {
			
			//Normal Search
			coins = TxPoWSearcher.searchCoins(	tip, true, 
												false, MiniData.ZERO_TXPOWID,
												false,MiniNumber.ZERO,
												false,MiniData.ZERO_TXPOWID, 
												true, tokenid, true);

		}else {
			
			//Special search
			coins = TxPoWSearcher.searchCoins(	tip, false, 
												false, MiniData.ZERO_TXPOWID,
												false, MiniNumber.ZERO,
												true, fromaddress, 
												false, MiniData.ZERO_TXPOWID, 
												false, "", true,
												false, Integer.MAX_VALUE,GeneralParams.IS_MEGAMMR);
			
		}
		
		//Get just this number..
		ArrayList<Coin> finalcoins = send.selectCoins(coins, tokenamount);
		
		//How much added..
		MiniNumber totaladded = MiniNumber.ZERO;
		for(Coin cc : finalcoins) {
			totaladded = totaladded.add(cc.getAmount());
		}
		
		//Is there change..
		MiniNumber change = totaladded.sub(tokenamount);
		
		//Do we have the cash
		if(change.isLess(MiniNumber.ZERO)) {
			MiniNumber total = totaladded;
			if(!tokenid.isEqual(Token.TOKENID_MINIMA)) {
				total = token.getScaledTokenAmount(total);
			}
			throw new CommandException("Not enough funds! Current balance : "+total);
		}		
		
		//OK - Now add all these coins..
		for(Coin cc : finalcoins) {
			trans.addInput(cc);
		}
		
		//And add the output
		if(!addonlychange) {
			String addr = getAddressParam("address");
			Coin maincoin = new Coin(new MiniData(addr), tokenamount, tokenid);
			
			//Do we need to add the Token..
			if(!tokenid.isEqual(Token.TOKENID_MINIMA)) {
				maincoin.setToken(token);
			}
			
			trans.addOutput(maincoin);
		}
		
		if(!change.isEqual(MiniNumber.ZERO)) {
			
			//Get a new address
			ScriptRow newwalletaddress = MinimaDB.getDB().getWallet().getDefaultAddress();
			MiniData chgaddress = new MiniData(newwalletaddress.getAddress());
			if(useaddress) {
				chgaddress = fromaddress;
			}
			
			//Change coin does not keep the state
			Coin changecoin = new Coin(Coin.COINID_OUTPUT, chgaddress, change, tokenid, false);
			if(!tokenid.isEqual(Token.TOKENID_MINIMA)) {
				changecoin.setToken(token);
			}
			
			//And finally.. add the change output
			trans.addOutput(changecoin);
		}
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(trans);
				
		//Calculate transid
		trans.calculateTransactionID();
				
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnaddamount();
	}

}










