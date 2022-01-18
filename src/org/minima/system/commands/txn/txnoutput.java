package org.minima.system.commands.txn;

import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.txn.txndb.TxnDB;
import org.minima.utils.json.JSONObject;

public class txnoutput extends Command {

	public txnoutput() {
		super("txnoutput","[id:] [amount:] [address:] (tokenid:) (storestate:) - Create a transaction output");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = TxnDB.getDB();
		
		//The transaction
		String id 			= getParam("id");
		MiniNumber amount	= getNumberParam("amount");
		MiniData address	= getDataParam("address");
		MiniData tokenid	= Token.TOKENID_MINIMA;
		if(existsParam("tokenid")) {
			tokenid	= getDataParam("tokenid");
		}
		
		//Create the Coin..
		Coin output = new Coin(Coin.COINID_OUTPUT, address, amount, tokenid);
		
		//Get the Transaction
		Transaction trans = db.getTransactionRow(id).getTransaction();
		trans.addOutput(output);
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnoutput();
	}

}
