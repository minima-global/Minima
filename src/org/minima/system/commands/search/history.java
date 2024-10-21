package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
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
				+ "action: (optional) default is 'list'\n"
				+ "	     list : List your transactions\n"
				+ "      size : Count how many transaction you have in History\n"
				+ "      customsize : use with 'where' to search TxPoWDB\n"
				+ "      transactions : How Many transactions in chain for 'depth' blocks"
				+ "\n"
				+ "max: (optional)\n"
				+ "    Maximum number of TxPoW to retrieve.\n"
				+ "\n"
				+ "offset: (optional)\n"
				+ "    Start the list from this point.\n"
				+ "\n"
				+ "depth: (optional)\n"
				+ "    How far down chain to search for transactions.\n"
				+ "\n"
				+ "relevant: (optional)\n"
				+ "    Do you want YOUR transactions or ALL transactions (defaults to true).\n"
				+ "\n"
				+ "where: (optional)\n"
				+ "    Use with customsize to search for a specific set. This is the WHERE clause in SQL query.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "history\n"
				+ "\n"
				+ "history max:20\n"
				+ "\n"
				+ "history action:size relevant:false\n"
				+ "\n"
				+ "history action:transactions depth:1720\n"
				+ "\n"
				+ "history action:customsize where:\"isblock=1 AND timemilli>1728037509020\"\n"
				+ "\n"
				+ "history max:20 offset:45\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"depth","max","offset","action","relevant","startmilli","where"}));
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
			
		}else if(action.equals("customsize")) {
			
			String where = getParam("where");
			
			int size = MinimaDB.getDB().getTxPoWDB().getSQLDB().customSizeQuery(where);
			resp.put("size", size);
			
		}else if(action.equals("transactions")) {
			
			int depth = getNumberParam("depth", new MiniNumber(1720)).getAsInt();
			
			//Now cycle down through the chain
			TxPoWTreeNode tip 		= MinimaDB.getDB().getTxPoWTree().getTip();
			MiniNumber startblock 	= tip.getBlockNumber();
			int count 		= 0;
			int totaltxns 	= 0;
			while(count<depth && tip!=null) {
				
				//Get the TxPoW.. 
				TxPoW txp = tip.getTxPoW();
				if(txp.isTransaction()) {
					totaltxns++;
				}
				
				totaltxns += txp.getTransactions().size();
				
				//Now move down..
				count++;
				tip = tip.getParent();
			}
			
			resp.put("startblock", startblock);
			resp.put("depth", count);
			resp.put("transactions", totaltxns);
			
		}else if(action.equals("list")) {
			
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
		
		}else{
			throw new CommandException("Invalid action:"+action);
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
