package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.TxBlock;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		
		//Get the archive db
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		JSONObject resp = new JSONObject();
		
		TxBlock lastblock = arch.loadLastBlock();
		if(lastblock != null) {
			resp.put("found", true);
			resp.put("block", lastblock.getTxPoW().getBlockNumber().toString());
			resp.put("hash", lastblock.getTxPoW().getTxPoWID());
		}else {
			resp.put("found", false);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new test();
	}

}
