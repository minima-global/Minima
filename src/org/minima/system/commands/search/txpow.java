package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txpow extends Command {

	public txpow() {
		super("txpow","(txpowid:) (onchain:) (block:) (address:) (relevant:) (max:) - Search for a specific TxPoW or check for onchain");
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
				+ "    TxPoW id of the TxPoW to search for.\n"
				 + "    Returns the txpow details.\n"
				+ "\n"
				+ "onchain: (optional)\n"
				+ "    TxPoW id to search for on chain. Must be in the unpruned chain.\n"
				+ "    Returns block info and number of confirmations.\n"
				+ "\n"
				+ "block: (optional)\n"
				+ "    Block number to search in. Must be in the unpruned chain.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    0x or Mx address. Search for TxPoWs containing this specific address.\n"
				+ "\n"
				+ "relevant: (optional)\n"
				+ "    true or false. Only list TxPoWs relevant to this node.\n"
				+ "\n"
				+ "max: (optional)\n"
				+ "    Max relevant TxPoW to retrieve. Default 100.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txpow txpowid:0x000..\n"
				+ "\n"
				+ "txpow block:200\n"
				+ "\n"
				+ "txpow address:0xCEF6..\n"
				+ "\n"
				+ "txpow onchain:0x000..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"txpowid","block","address","onchain","relevant","max"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the txpowid
		if(existsParam("txpowid")) {
			String txpowid = getAddressParam("txpowid");
			
			//Search for a given txpow
			TxPoW txpow = MinimaDB.getDB().getTxPoWDB().getTxPoW(txpowid);
			if(txpow == null) {
				
				//Lets check the archive..
				TxBlock block = MinimaDB.getDB().getArchive().loadBlock(txpowid);
				if(block!=null) {
					txpow = block.getTxPoW();
				}else {
					
					//Lets check the Mysql..
					boolean autologindetail = MinimaDB.getDB().getUserDB().getAutoLoginDetailsMySQL();
					if(autologindetail) {
					
						//Run a search of the MySQL
						String command 		= "mysql action:findtxpow txpowid:"+txpowid;
						JSONArray res 		= CommandRunner.getRunner().runMultiCommand(command);
						JSONObject result 	= (JSONObject) res.get(0);
						
						if((boolean) result.get("found")) {
							ret.put("response", result.get("txpow"));
							return ret;
						}
					}else {
						throw new CommandException("TxPoW not found : "+txpowid);
					}
				}
			}
		
			ret.put("response", txpow.toJSON());
			
		}else if(existsParam("relevant")) {
			
			int max = getNumberParam("max",TxPoWSqlDB.MAX_RELEVANT_TXPOW).getAsInt();
			
			ArrayList<TxPoW> txps = MinimaDB.getDB().getTxPoWDB().getSQLDB().getAllRelevant(max);
			
			//Only add them once..
			JSONArray txns = new JSONArray();
			HashSet<String> allreadyadded = new HashSet<>();
			for(TxPoW txp : txps) {
				String txpowid = txp.getTxPoWID();
				if(!allreadyadded.contains(txpowid)) {
					allreadyadded.add(txpowid);
					txns.add(txp.toJSON());
				}
			}
			
//			JSONArray txns = new JSONArray();
//			for(TxPoW txp : txps) {
//				txns.add(txp.toJSON());
//			}
			
			ret.put("response", txns);
			
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
				
				//Search the Archive..
				TxBlock txblock = MinimaDB.getDB().getArchive().loadBlockFromNumber(block);
				if(txblock == null) {
					throw new CommandException("TxPoW not found @ height "+block);
				}
				
				txpow = txblock.getTxPoW();
			}
			
			ret.put("response", txpow.toJSON());
			
		}else if(existsParam("address")) {
			
			String address = getAddressParam("address");
			
			ArrayList<TxPoW> txps = TxPoWSearcher.searchTxPoWviaAddress(new MiniData(address));
			
			//Only add them once..
			JSONArray txns = new JSONArray();
			HashSet<String> allreadyadded = new HashSet<>();
			for(TxPoW txp : txps) {
				String txpowid = txp.getTxPoWID();
				if(!allreadyadded.contains(txpowid)) {
					allreadyadded.add(txpowid);
					txns.add(txp.toJSON());
				}
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
