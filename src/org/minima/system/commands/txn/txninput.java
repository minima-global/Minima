package org.minima.system.commands.txn;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txninput extends Command {

	public txninput() {
		super("txninput","[id:] [coinid:] (floating:true|false) - Add a coin as an input to a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id = getParam("id");
		
		//The Coin
		String coinid = getParam("coinid");
		
		//Get the coin
		Coin cc = TxPoWSearcher.searchCoin(new MiniData(coinid));
		if(cc == null) {
			throw new CommandException("CoinID not found : "+coinid);
		}
		
		//Is it a floating input..
		boolean eltoo = getBooleanParam("floating",false);
		if(eltoo) {
			//Check thisa coin is floating..
			if(!cc.isFloating()) {
				throw new CommandException("Coin cannot be ELTOO as is not floating.. "+cc.toJSON().toString());
			}
			
			cc.resetCoinID(Coin.COINID_ELTOO);
		}
		
		//Get the Transaction
		Transaction trans = db.getTransactionRow(id).getTransaction();
		trans.addInput(cc);
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txninput();
	}

}
