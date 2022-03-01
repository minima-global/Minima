package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txnmmr extends Command {

	public txnmmr() {
		super("txnmmr","[id:] [coinid:0x..|auto] - Add MMR Proofs");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id 		= getParam("id");
		String coinid	= getParam("coinid");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		if(coinid.equals("auto")) {
			
			//Cycle through and add all the MMR proofs we have..
			
			
			
		}
		
		JSONObject resp = new JSONObject();
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnmmr();
	}

}
