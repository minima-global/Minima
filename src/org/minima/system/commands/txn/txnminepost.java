package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class txnminepost extends Command {

	public txnminepost() {
		super("txnminepost","[data:] - Post a pre-mined transaction");
	}
	
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		MiniData txdata = getDataParam("data");
		
		//Convert to a TXPOW
		TxPoW txp = TxPoW.convertMiniDataVersion(txdata);
		
		//Now Post it!
		Main.getInstance().PostMessage(new Message(Main.MAIN_TXPOWMINED).addObject("txpow", txp));
		
		//Return the MINED txn..
		JSONObject resp = new JSONObject();
		resp.put("data", txp.toJSON());
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnminepost();
	}

}
