package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnoutput extends Command {

	public txnoutput() {
		super("txnoutput","[id:] [amount:] [address:] (tokenid:) (storestate:) - Create a transaction output");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnoutput\n"
				+ "\n"
				+ "Create a transaction output.\n"
				+ "\n"
				+ "This will create a new coin (UTxO).\n"
				+ "\n"
				+ "If the sum of inputs > outputs, the difference will be burned unless change to the sender is defined as an output.\n"
				+ "\n"
				+ "Optionally store the transaction state variables in the new output coin.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction to add an output to.\n"
				+ "\n"
				+ "amount:\n"
				+ "    The amount for the output. To send to the specified address.\n"
				+ "\n"
				+ "address:\n"
				+ "    Address of the recipient/script to send the output to. Can be 0x or Mx address.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    tokenid of the output. Default is Minima (0x00).\n"
				+ "\n"
				+ "storestate: (optional)\n"
				+ "    true or false, true will keep the state variables of the transaction in the newly created output coin.\n"
				+ "    Default is true.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnoutput id:simpletxn amount:10 address:0xFED5..\n"
				+ "\n"
				+ "txnoutput id:multisig amount:10 address:0xFED5.. tokenid:0xCEF5.. storestate:false\n"
				+ "\n"
				+ "txnoutput id:eltootxn amount:10 address:0xFED5..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","amount","address","tokenid","storestate"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 			= getParam("id");
		MiniNumber amount	= getNumberParam("amount");
		MiniData address	= new MiniData(getAddressParam("address"));
		boolean storestate 	= getBooleanParam("storestate", true);
		
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
		
		//Create the Coin..
		Coin output = new Coin(Coin.COINID_OUTPUT, address, miniamount, tokenid,storestate);
		if(token != null) {
			output.setToken(token);
		}
		
		//Get the Transaction
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		Transaction trans = txnrow.getTransaction();
		trans.addOutput(output);
		
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
		return new txnoutput();
	}

}
