package org.minima.system.commands.send;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.ScriptProof;
import org.minima.objects.Transaction;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class sendpost extends Command {

	public sendpost() {
		super("sendpost","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String txnfile = getParam("file");
		
		//Load the txn
		byte[] data = MiniFile.readCompleteFile(MiniFile.createBaseFile(txnfile));
		
		//Create the MininData
		MiniData txndata = new MiniData(data);
		
		//Now convert back into a TxPoW
		TxPoW txp = TxPoW.convertMiniDataVersion(txndata);
		
		//Calculate the TxPOWID
		txp.calculateTXPOWID();
				
		JSONObject sigtran = new JSONObject();
		sigtran.put("txpow", txp.toJSON());
		
		JSONObject resp = new JSONObject();
		ret.put("response", sigtran);
		
		//Post It..!
		Main.getInstance().getTxPoWMiner().mineTxPoWAsync(txp);
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new sendpost();
	}
}
