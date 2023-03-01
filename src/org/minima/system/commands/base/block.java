package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class block extends Command {

	public block() {
		super("block","Simply return the current top block");
	}
	
	@Override
	public String getFullHelp() {
		return "\nblock\n"
				+ "\n"
				+ "Return the top block\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "block\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the top block..
		TxPowTree tree 		= MinimaDB.getDB().getTxPoWTree();
		TxPoWTreeNode tip 	= tree.getTip();
		TxPoW topblock 		= tip.getTxPoW();
		
		JSONObject resp = new JSONObject();
		resp.put("block", topblock.getBlockNumber().toString());
		resp.put("hash", topblock.getTxPoWID());
		resp.put("timemilli", topblock.getTimeMilli().toString());
		resp.put("date", new Date(topblock.getTimeMilli().getAsLong()).toString());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new block();
	}

}
