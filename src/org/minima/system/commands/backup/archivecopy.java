package org.minima.system.commands.backup;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class archivecopy extends Command {

	public archivecopy() {
		super("archivecopy","utility function to copy over the complete archive DB");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		MiniNumber top = getNumberParam("topblock",MiniNumber.THOUSAND);
		
		//First lets check the archive DB
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		MiniNumber range 	= new MiniNumber(100);
		MiniNumber lasttop 	= top.decrement(); 
		
		int count = 0;
		while(count++ < 4) {
			MinimaLogger.log("Loading from "+top);
			ArrayList<TxBlock> blocks = arch.loadBlockRange(top.sub(range), top);
			
			for(TxBlock block : blocks) {
				MiniNumber bnum = block.getTxPoW().getBlockNumber();
				
				if(!bnum.isEqual(lasttop)) {
					MinimaLogger.log("WRONG Block "+block.getTxPoW().getBlockNumber());
				}
				
				lasttop = lasttop.decrement();
			}
			
			top 	= top.sub(range);
			lasttop = top.decrement();
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new archivecopy();
	}

}
