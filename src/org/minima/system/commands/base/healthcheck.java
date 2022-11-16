package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.cascade.Cascade;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class healthcheck extends Command {

	public healthcheck() {
		super("healthcheck","Run a system check to see everything adds up");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		JSONObject resp = new JSONObject();
		
		//First get the tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		if(tip == null) {
			throw new CommandException("No TIP block found.. ?");
		}
		
		JSONObject chain = new JSONObject();
		
		chain.put("tip", tip.getTxPoW().getBlockNumber().toString());
		
		TxPoWTreeNode root = MinimaDB.getDB().getTxPoWTree().getRoot();
		MiniNumber treeroot = root.getTxPoW().getBlockNumber();
		chain.put("root", treeroot.toString());
		
		MiniNumber len = tip.getTxPoW().getBlockNumber().sub(treeroot);
		chain.put("chainlength", len.toString());
		
		resp.put("chain", chain);
		
		//Now check the cascade
		Cascade casc = MinimaDB.getDB().getCascade();
		MiniNumber casctip = casc.getTip().getTxPoW().getBlockNumber();
		
		JSONObject cascade = new JSONObject();
		cascade.put("tip", casctip.toString());
		
		boolean correctstart = casctip.isEqual(treeroot.decrement());
		cascade.put("tipcorrect", correctstart);
		
		cascade.put("cascadelength", casc.getLength());
		
		resp.put("cascade", cascade);
		
		//Now check Maxima..
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		JSONObject maxima = new JSONObject();
		
		int hosts 	 = maxdb.getAllHosts().size();
		int contacts = maxdb.getAllContacts().size();
		
		maxima.put("hosts", hosts);
		maxima.put("contacts", contacts);
		
		resp.put("maxima", maxima);
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new healthcheck();
	}

}
