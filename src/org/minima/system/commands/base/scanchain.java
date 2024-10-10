package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class scanchain extends Command {

	public scanchain() {
		super("scanchain","(depth:) - Scan back through the chain and see every address used");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"depth"}));
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
		
		//How deep..
		int depth = getNumberParam("depth", new MiniNumber(16)).getAsInt();
		
		//Now search back through the chain
		JSONArray blockdata = new JSONArray();
		int counter = 0;
		while(tip != null && counter<depth) {
			
			TxPoW topblock = tip.getTxPoW();
			
			JSONObject blockjson = new JSONObject();
			blockjson.put("number", topblock.getBlockNumber());
			blockjson.put("timemilli", topblock.getTimeMilli());
			blockjson.put("date", new Date(topblock.getTimeMilli().getAsLong()));
			blockjson.put("txpowid", topblock.getTxPoWID());
			
			JSONArray inputs  = new JSONArray();
			JSONArray outputs = new JSONArray();
			
			//Scan it..
			checkTxPoWAddresses(topblock, inputs, outputs);
			
			//Now scan all txpow in the block
			ArrayList<MiniData> alltrans = topblock.getBlockTransactions();
			for(MiniData txid : alltrans) {
				TxPoW txpow = MinimaDB.getDB().getTxPoWDB().getTxPoW(txid.to0xString());
				if(txpow != null) {
					//Scan it..
					checkTxPoWAddresses(txpow, inputs, outputs);
				}
			}
			
			//Add data..
			blockjson.put("inputs", inputs);
			blockjson.put("outputs", outputs);
			
			//And add to final list
			blockdata.add(blockjson);
			
			//And move to the next block
			tip = tip.getParent();
			counter++;
		}
		
		JSONObject resp = new JSONObject();
		resp.put("depth", depth);
		resp.put("blocks", blockdata);
		ret.put("response", resp);
		
		return ret;
	}

	public void checkTxPoWAddresses(TxPoW zTxPOW, JSONArray zInputs,JSONArray zOutputs) {
		
		if(zTxPOW.isTransaction()) {
			
			//Get the transaction
			Transaction trans = zTxPOW.getTransaction();
			
			//Get the inputs
			ArrayList<Coin> ins = trans.getAllInputs();
			for(Coin cc : ins) {
				JSONObject cjson = cc.toJSON();
				cjson.put("spent", true);
				cjson.put("txpowid", zTxPOW.getTxPoWID());
				zInputs.add(cjson);
			}
			
			//Get the outputs
			ArrayList<Coin> outs = trans.getAllOutputs();
			for(Coin cc : outs) {
				JSONObject cjson = cc.toJSON();
				cjson.put("txpowid", zTxPOW.getTxPoWID());
				zOutputs.add(cjson);
			}
		}
	}
	
	@Override
	public Command getFunction() {
		return new scanchain();
	}

}
