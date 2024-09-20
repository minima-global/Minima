package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class history extends Command {

	public history() {
		super("history","(action:) (max:) (offset:) (relevant:) - Search for all relevant TxPoW");
	}
	
	@Override
	public String getFullHelp() {
		return "\nhistory\n"
				+ "\n"
				+ "Return all TxPoW relevant to you. Default to 100 max (can be slow)\n"
				+ "\n"
				+ "action: list or size (optional)\n"
				+ "    Get list or check total size.\n"
				+ "\n"
				+ "max: (optional)\n"
				+ "    Maximum number of TxPoW to retrieve.\n"
				+ "\n"
				+ "offset: (optional)\n"
				+ "    Start the list from this point.\n"
				+ "\n"
				+ "relevant: (optional)\n"
				+ "    Do you want YOUR transactions or ALL transactions (defaults to true).\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "history\n"
				+ "\n"
				+ "history max:20\n"
				+ "\n"
				+ "history max:20 offset:45\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"max","offset","action","relevant","startmilli"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
	
		String action 	= getParam("action", "list");
		JSONObject resp = new JSONObject();
		
		boolean relevant = getBooleanParam("relevant",true);
		
		if(action.equals("size")) {
			
			if(relevant) {
				int size = MinimaDB.getDB().getTxPoWDB().getSQLDB().getRelevantSize();
				resp.put("size", size);
			
			}else {
				
				long oneday 		= 1000 * 60 * 60 * 24;
				long defaultstart 	= System.currentTimeMillis() - oneday; 
				long starttime 		= getNumberParam("startmilli", new MiniNumber(defaultstart)).getAsLong();
				int size 			= MinimaDB.getDB().getTxPoWDB().getSQLDB().getLatestTxPoWSize(starttime);
				resp.put("startmilli", starttime);
				resp.put("size", size);
			}
			
		}else {
			
			int max 	= getNumberParam("max",TxPoWSqlDB.MAX_RELEVANT_TXPOW).getAsInt();
			int offset 	= getNumberParam("offset",MiniNumber.ZERO).getAsInt();
			
			ArrayList<TxPoW> txps;
			if(relevant) {
				txps = MinimaDB.getDB().getTxPoWDB().getSQLDB().getAllRelevant(max,offset);
			}else {
				txps = MinimaDB.getDB().getTxPoWDB().getSQLDB().getLatestTxPoW(max,offset);
			}
				
			JSONArray txns 			= new JSONArray();
			JSONArray txndetails 	= new JSONArray();
			for(TxPoW txp : txps) {
				txns.add(txp.toJSON());
				txndetails.add(getTxnDetails(txp));
			}
			
			resp.put("relevant", relevant);
			resp.put("txpows", txns);
			resp.put("details", txndetails);
			resp.put("size", txns.size());
		}
		
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
