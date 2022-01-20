package org.minima.system.commands.txn;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class txnsign extends Command {

	public txnsign() {
		super("txnsign","[id:] [publickey:] - Sign a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id 	= getParam("id");
		String pubk	= getParam("publickey");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		Transaction txn = txnrow.getTransaction();
		Witness wit		= txnrow.getWitness();
		
		//Calculate the TransactionID..
		MiniData transid = Crypto.getInstance().hashObject(txn);
		
		//Get the Private key..
		Wallet walletdb = MinimaDB.getDB().getWallet();
		KeyRow pubrow 	= walletdb.getKeysRowFromPublicKey(pubk);
		if(pubrow == null) {
			throw new CommandException("Public Key not found");
		}
		
		//Use the wallet..
		Signature signature = walletdb.sign(pubrow.getPrivateKey(), transid);
			
		//Add it..
		wit.addSignature(signature);
		
		JSONObject resp = new JSONObject();
		ret.put("response", txnrow.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnsign();
	}

}
