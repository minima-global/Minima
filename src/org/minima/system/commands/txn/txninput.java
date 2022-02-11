package org.minima.system.commands.txn;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txninput extends Command {

	public txninput() {
		super("txninput","[id:] (coinid:) (coindata:) (floating:true|false) (address:) (amount:) (tokenid:) - Add a coin as an input to a transaction");
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
				//Check this coin is floating..
				if(!cc.isFloating()) {
					throw new CommandException("Coin cannot be ELTOO as is not floating.. "+cc.toJSON().toString());
				}
				
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
				//Check this coin is floating..
				if(!cc.isFloating()) {
					throw new CommandException("Coin cannot be ELTOO as is not floating.. "+cc.toJSON().toString());
				}
				
				cc.resetCoinID(Coin.COINID_ELTOO);
			}
		}else {
			
			//Is it a floater..
			if(!eltoo) {
				throw new CommandException("Coin MUST be floating if no coinid or coindata specified");
			}
			
			//Get the details..
			String address  = getParam("address");
			String amount   = getParam("amount");
			String tokenid  = getParam("tokenid","0x00");
			
			//Create a COIN..
			cc = new Coin(new MiniData(address), new MiniNumber(amount), new MiniData(tokenid));
			cc.setFloating(true);
			cc.resetCoinID(Coin.COINID_ELTOO);
			
		}
		
		//Get the transaction..
		Transaction trans = txnrow.getTransaction();
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
