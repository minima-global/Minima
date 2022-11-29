package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txpow extends Command {

	public txpow() {
		super("txpow","(txpowid:txpowid) (onchain:) (block:) (address:) - Search for a specific TxPoW or check for onchain");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxpow\n"
				+ "\n"
				+ "Search for a specific TxPoW in the unpruned chain or your mempool.\n"
				+ "\n"
				+ "Search by txpowid, block or 0x / Mx address.\n"
				+ "\n"
				+ "txpowid: (optional)\n"
				+ "    txpowid of the TxPoW to search for.\n"
				+ "\n"
				+ "block: (optional)\n"
				+ "    Block number to search in. Must be in the unpruned chain.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    0x or Mx address. Search for TxPoWs containing this specific address.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txpow txpowid:0x000..\n"
				+ "\n"
				+ "txpow block:200\n"
				+ "\n"
				+ "txpow address:0xCEF6..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"txpowid","block","address","onchain"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the txpowid
		if(existsParam("txpowid")) {
			String txpowid = getParam("txpowid");
			
			//Search for a given txpow
			TxPoW txpow = MinimaDB.getDB().getTxPoWDB().getTxPoW(txpowid);
			if(txpow == null) {
				throw new CommandException("TxPoW not found : "+txpowid);
			}
		
			ret.put("response", txpow.toJSON());
			
		}else if(existsParam("onchain")) {
			MiniData txpowid = getDataParam("onchain");
		
			JSONObject resp = new JSONObject();
			
			TxPoW block = TxPoWSearcher.searchChainForTxPoW(txpowid);
			if(block == null) {
				resp.put("found", false);
			}else {
				
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				
				resp.put("found", true);
				resp.put("block", block.getBlockNumber().toString());
				resp.put("blockid", block.getTxPoWID());
				resp.put("tip", tip.getBlockNumber().toString());
				
				MiniNumber depth  = tip.getBlockNumber().sub(block.getBlockNumber());
				resp.put("confirmations", depth.toString());
			}
			
			ret.put("response", resp);
		
		}else if(existsParam("block")) {
			
			MiniNumber block = getNumberParam("block");
			
			TxPoW txpow = TxPoWSearcher.getTxPoWBlock(block);
			if(txpow == null) {
				throw new CommandException("TxPoW not found @ height "+block);
			}
			
			ret.put("response", txpow.toJSON());
			
		}else if(existsParam("address")) {
			
			String address = getAddressParam("address");
			
			ArrayList<TxPoW> txps = TxPoWSearcher.searchTxPoWviaAddress(new MiniData(address));
			
			JSONArray txns = new JSONArray();
			for(TxPoW txp : txps) {
				txns.add(txp.toJSON());
			}
			
			ret.put("response", txns);
			
		}else {
			throw new CommandException("Must Specify search params");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txpow();
	}

}
