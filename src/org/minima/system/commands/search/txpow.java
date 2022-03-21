package org.minima.system.commands.search;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
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
		super("txpow","(txpowid:txpowid) (block:) (address:) - Search for a specific TxPoW");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the txpowid
		if(existsParam("txpowid")) {
			String txpowid = getParam("txpowid", "0x01");
			
			//Search for a given txpow
			TxPoW txpow = MinimaDB.getDB().getTxPoWDB().getTxPoW(txpowid);
			if(txpow == null) {
				throw new CommandException("TxPoW not found : "+txpowid);
			}
		
			ret.put("response", txpow.toJSON());
			
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
