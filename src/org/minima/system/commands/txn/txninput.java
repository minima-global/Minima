package org.minima.system.commands.txn;

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
		super("txninput","[id:] (coinid:) (coindata:) (floating:) (address:) (amount:) (tokenid:) (sciptmmr:true)- Add a coin as an input to a transaction");
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
