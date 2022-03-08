package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class burn extends Command {

	public burn() {
		super("burn","(action:list|default) - View 'burn' metrics or set default");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "list");
		
		JSONObject response = new JSONObject();
		
		if(action.equals("list")) {
			
			MiniNumber minburn = MiniNumber.BILLION;
			MiniNumber maxburn = MiniNumber.ZERO;
			
			//Get the tip..
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				
			//Get the 
			TxPoW tiptxpow = tip.getTxPoW();
		
			boolean found = false;
			if(tiptxpow.isTransaction()) {
				found = true;
				
				//Get the transaction burn..
				Transaction trans 	= tiptxpow.getTransaction();
				MiniNumber burn 	= trans.getBurn();
				if(burn.isMore(maxburn)) {
					maxburn = burn;
				}
				if(burn.isLess(minburn)) {
					minburn = burn;
				}
				
				//Get the transaction burn..
				trans 	= tiptxpow.getBurnTransaction();
				burn 	= trans.getBurn();
				if(burn.isMore(maxburn)) {
					maxburn = burn;
				}
				if(burn.isLess(minburn)) {
					minburn = burn;
				}
				
			}
			
			//Did we find one
			if(!found) {
				minburn = MiniNumber.ZERO;
				maxburn = MiniNumber.ZERO;
			}
			
			JSONObject parent = new JSONObject();
			parent.put("max", maxburn);
			parent.put("min", minburn);
			
			response.put("1block",parent);
				
		}
		
		
		ret.put("response", response);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new burn();
	}

}
