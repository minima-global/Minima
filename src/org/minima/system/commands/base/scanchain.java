package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class scanchain extends Command {

	public scanchain() {
		super("scanchain","(depth:) (offset:) - Scan back through the chain and see all transaction data");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"depth","offset"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the top block..
		TxPowTree tree 		= MinimaDB.getDB().getTxPoWTree();
		TxPoWTreeNode tip 	= tree.getTip();
		if(tip == null) {
			throw new CommandException("NO Blocks yet..");
		}
		
		//What offset
		int counter = 0;
		JSONArray blockdata = new JSONArray();
		
		//Offset
		int offset = getNumberParam("offset", MiniNumber.ZERO).getAsInt();
		
		//How deep..
		int depth = getNumberParam("depth", new MiniNumber(16)).getAsInt();
				
		//First do the offset
		while(tip != null && counter<=offset) {
			tip = tip.getParent();
			counter++;
		}
		
		if(tip == null) {
			JSONObject resp = new JSONObject();
			resp.put("depth", depth);
			resp.put("offset", offset);
			resp.put("blocks", blockdata);
			ret.put("response", resp);
			
			return ret;
		}
		
		//Now search back through the chain
		MiniNumber startblock = tip.getBlockNumber(); 
		counter = 0;
		while(tip != null && counter<=depth) {
			
			TxPoW topblock = tip.getTxPoW();
			
			JSONObject blockjson = new JSONObject();
			blockjson.put("block", topblock.getBlockNumber());
			blockjson.put("depth", startblock.sub(topblock.getBlockNumber()));
			blockjson.put("timemilli", topblock.getTimeMilli());
			blockjson.put("date", new Date(topblock.getTimeMilli().getAsLong()).toString() );
			blockjson.put("txpowid", topblock.getTxPoWID());
			
			//All the transaction data in the block
			JSONArray transactiondata  = new JSONArray();
			
			//Is this block a transaction
			if(topblock.isTransaction()) {
				transactiondata.add(getTransactionDetails(topblock));
			}
			
			//Add all the transactions
			ArrayList<MiniData> alltrans = topblock.getBlockTransactions();
			for(MiniData txid : alltrans) {
				TxPoW txpow = MinimaDB.getDB().getTxPoWDB().getTxPoW(txid.to0xString());
				if(txpow != null) {
					//Scan it..
					transactiondata.add(getTransactionDetails(txpow));
				}
			}
			
			blockjson.put("transactions", transactiondata);
			
			//And add to final list
			blockdata.add(blockjson);
			
			//And move to the next block
			tip = tip.getParent();
			counter++;
		}
		
		JSONObject resp = new JSONObject();
		resp.put("depth", depth);
		resp.put("offset", offset);
		resp.put("blocks", blockdata);
		ret.put("response", resp);
		
		return ret;
	}
	
	public JSONObject getTransactionDetails(TxPoW zTxPoW) {
		
		JSONObject ret = new JSONObject();
		ret.put("txpowid",zTxPoW.getTxPoWID());
		ret.put("istransaction",!zTxPoW.getTransaction().isEmpty());
		if(!zTxPoW.getTransaction().isEmpty()) {
			ret.put("transaction",zTxPoW.getTransaction().toJSON());
		}
		
		ret.put("isburntransaction",!zTxPoW.getBurnTransaction().isEmpty());
		if(!zTxPoW.getBurnTransaction().isEmpty()) {
			ret.put("burntransaction",zTxPoW.getBurnTransaction().toJSON());
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new scanchain();
	}

}
