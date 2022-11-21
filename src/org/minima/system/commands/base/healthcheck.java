package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.cascade.Cascade;
import org.minima.database.cascade.CascadeNode;
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
	public String getFullHelp() {
		return "\nhealthcheck\n"
				+ "\n"
				+ "Return information about your chain, cascade and maxima.\n"
				+ "\n"
				+ "Chain - tip:current chain tip block, root:current chain root block, chainlength:number of blocks in the heaviest chain.\n"
				+ "\n"
				+ "Cascade - tip:current cascade tip block, tipcorrect:returns true if the cascade tip meets the root of the txpow tree.\n"
				+ "\n"
				+ "Maxima - hosts:number of maxima hosts, contacts:number of maxima contacts.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "healthcheck\n";
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
		CascadeNode ctip = casc.getTip();
		if(ctip != null) {
			MiniNumber casctip = casc.getTip().getTxPoW().getBlockNumber();
			
			JSONObject cascade = new JSONObject();
			cascade.put("tip", casctip.toString());
			
			boolean correctstart = casctip.isEqual(treeroot.decrement());
			cascade.put("tipcorrect", correctstart);
			
			cascade.put("cascadelength", casc.getLength());
			
			resp.put("cascade", cascade);
		}else {
			resp.put("cascade", "nocacade");
		}
		
		
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
