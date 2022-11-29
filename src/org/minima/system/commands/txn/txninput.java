package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txninput extends Command {

	public txninput() {
		super("txninput","[id:] (coinid:) (coindata:) (floating:) (address:) (amount:) (tokenid:) (scriptmmr:true)- Add a coin as an input to a transaction");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxninput\n"
				+ "\n"
				+ "Add a coin as an input to a transaction.\n"
				+ "\n"
				+ "Optionally specify address, amount and tokenid to use an unspecified coinid - a floating ELTOO coin.\n"
				+ "\n"
				+ "A floating coin can be attached to multiple different existing coins as long as it has the same\n"
				+ "\n"
				+ "address, amount and tokenid - but different coinid.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction to add an input to.\n"
				+ "\n"
				+ "coinid: (optional)\n"
				+ "    The id of the coin to add as an input.\n"
				+ "\n"
				+ "coindata: (optional)\n"
				+ "    The data of the coin to add, instead of coinid.\n"
				+ "    Can be from the 'coinexport' command or 'outputcoindata' from another transaction.\n"
				+ "\n"
				+ "floating: (optional)\n"
				+ "    true or false, true will add an unspecified, floating ELTOO coin as an input.\n"
				+ "    If true, also specify address, amount, tokenid.\n"
				+ "    If false, specify a coinid or coindata.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    Coin address to use for the floating input. Can be 0x or Mx address.\n"
				+ "    The coin that is used "
				+ "\n"
				+ "amount: (optional)\n"
				+ "    Amount of a coin for the floating input.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    tokenid of a coin for the floating input.\n"
				+ "\n"
				+ "scriptmmr: (optional)\n"
				+ "    true or false, true will add the scripts and MMR proof for the coin.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txninput id:simpletxn coinid:0xD0BF..\n"
				+ "\n"
				+ "txninput id:multisig coinid:0xD0BF.. scriptmmr:true\n"
				+ "\n"
				+ "txninput id:posttxn coindata:0x000..\n"
				+ "\n"
				+ "txninput id:eltootxn floating:true address:0xFED5.. amount:10 tokenid:0x00\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","coinid",
				"coindata","floating","address","amount","tokenid","scriptmmr"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		//The transaction
		String id = getParam("id");
		
		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		//Is it a floating input..
		boolean eltoo = getBooleanParam("floating",false);
		
		//The Coin
		Coin cc = null;
		if(existsParam("coinid")) {
			
			String coinid = getParam("coinid");
			
			//Get the coin
			cc = TxPoWSearcher.searchCoin(new MiniData(coinid));
			if(cc == null) {
				throw new CommandException("CoinID not found : "+coinid);
			}
			
			//Is it a floating input..
			if(eltoo) {
				cc.resetCoinID(Coin.COINID_ELTOO);
			}
			
		}else if(existsParam("coindata")){
			
			String coindata = getParam("coindata");
			
			//Use the coindata version..
			cc= Coin.convertMiniDataVersion(new MiniData(coindata));
			if(cc == null) {
				throw new CommandException("ERROR importing coin data");
			}
			
			//Is it a floating input..
			if(eltoo) {
				cc.resetCoinID(Coin.COINID_ELTOO);
			}
		}else {
			
			//Get the details..
			String address  = getAddressParam("address");
			String amount   = getParam("amount");
			String tokenid  = getParam("tokenid","0x00");
			
			//Create a COIN..
			cc = new Coin(Coin.COINID_ELTOO, new MiniData(address), new MiniNumber(amount), new MiniData(tokenid));
		}
		
		//Get the transaction..
		Transaction trans = txnrow.getTransaction();
		trans.addInput(cc);
		
		//Calculate the correct CoinID - if possible..
		TxPoWGenerator.precomputeTransactionCoinID(trans);
		
		//Calculate transid
		trans.calculateTransactionID();
		
		//Are we adding the scripts and MMR for this coin..
		boolean smmr = getBooleanParam("scriptmmr", false);
		if(smmr) {
			//Add the details for this coin..
			txnutils.setMMRandScripts(cc, txnrow.getWitness());
		}
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txninput();
	}

}
