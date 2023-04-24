package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class history extends Command {

	public history() {
		super("history","Search for all relevant TxPoW");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
			
		ArrayList<TxPoW> txps = MinimaDB.getDB().getTxPoWDB().getSQLDB().getAllRelevant();
		
		JSONArray txns 			= new JSONArray();
		JSONArray txndetails 	= new JSONArray();
		for(TxPoW txp : txps) {
			txns.add(txp.toJSON());
			txndetails.add(getTxnDetails(txp));
		}
	
		JSONObject resp = new JSONObject();
		resp.put("txpows", txns);
		resp.put("details", txndetails);
		resp.put("size", txns.size());
		
		ret.put("response", resp);
		
		return ret;
	}

	public JSONObject getTxnDetails(TxPoW zTxPoW) {
		
		JSONObject ret = new JSONObject();
		
		//Hashtable of Amounts
		Hashtable<String, MiniNumber> inamounts  = new Hashtable<>();
		Hashtable<String, MiniNumber> outamounts = new Hashtable<>();
		
		//Get the Transaction
		Transaction trans = zTxPoW.getTransaction();
		
		//Get the wallet..
		Wallet wal = MinimaDB.getDB().getWallet();
		
		//Cycle through the Inputs.. are they relevant
		ArrayList<Coin> inputs = trans.getAllInputs();
		for(Coin cc : inputs) {
			
			//Do we add it..
			String addr = cc.getAddress().to0xString();
			String tok  = cc.getTokenID().to0xString();
			
			if(wal.isAddressRelevant(addr)) {
				//Do we have it..
				MiniNumber tot = inamounts.get(tok);
				if(tot == null) {
					tot = MiniNumber.ZERO;
				}
				
				//Add to the correct token value
				if(tok.equals("0x00")) {
					tot = tot.add(cc.getAmount());
				}else {
					tot = tot.add(cc.getTokenAmount());
				}
				
				//And add back
				inamounts.put(tok, tot);
			}
		}
		
		//Cycle through the Outputs.. are they relevant
		ArrayList<Coin> outputs = trans.getAllOutputs();
		for(Coin cc : outputs) {
			
			//Do we add it..
			String addr = cc.getAddress().to0xString();
			String tok  = cc.getTokenID().to0xString();
			
			if(wal.isAddressRelevant(addr)) {
				//Do we have it..
				MiniNumber tot = outamounts.get(tok);
				if(tot == null) {
					tot = MiniNumber.ZERO;
				}
				
				//Add to the correct token value
				if(tok.equals("0x00")) {
					tot = tot.add(cc.getAmount());
				}else {
					tot = tot.add(cc.getTokenAmount());
				}
				
				//And add back
				outamounts.put(tok, tot);
			}
		}
		
		//HashSet of all the different tokens..
		HashSet<String> alltoks = new HashSet<>();
		
		//Now Cycle through the amounts..
		JSONObject ins = new JSONObject();
		Enumeration<String> inkeys = inamounts.keys();
		while(inkeys.hasMoreElements()) {
			String key 		= inkeys.nextElement();
			MiniNumber amt 	= inamounts.get(key);
			ins.put(key, amt.toString());
			
			alltoks.add(key);
		}
		
		JSONObject outs = new JSONObject();
		Enumeration<String> outkeys = outamounts.keys();
		while(outkeys.hasMoreElements()) {
			String key 		= outkeys.nextElement();
			MiniNumber amt 	= outamounts.get(key);
			outs.put(key, amt.toString());
			
			alltoks.add(key);
		}
		
		//Finally calculate the difference...
		JSONObject diffs = new JSONObject();
		for(String key : alltoks) {
			
			//The input..
			MiniNumber in = inamounts.get(key);
			if(in == null) {
				in = MiniNumber.ZERO;
			}
			
			//The output
			MiniNumber out = outamounts.get(key);
			if(out == null) {
				out = MiniNumber.ZERO;
			}
			
			//Now the difference..
			MiniNumber difference = out.sub(in); 
			
			//And add..
			diffs.put(key, difference.toString());
		}
		
		//Add to the JSON
		ret.put("inputs", ins);
		ret.put("outputs", outs);
		ret.put("difference", diffs);
		
		return ret;
	}
	
	
	@Override
	public Command getFunction() {
		return new history();
	}

}
