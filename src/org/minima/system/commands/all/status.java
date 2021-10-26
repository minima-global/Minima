package org.minima.system.commands.all;

import java.math.BigDecimal;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.network.NetworkManager;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONObject;

public class status extends Command {

	public status() {
		super("status","Show general status for Minima");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Quick clean
		System.gc();
		
		JSONObject details = new JSONObject();
		details.put("time", new Date().toString());
		details.put("version", GlobalParams.MINIMA_VERSION);
		details.put("configuration", GeneralParams.CONFIGURATION_FOLDER);
		
		//The Database
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		ArchiveManager arch = MinimaDB.getDB().getArchive(); 
		
		JSONObject database = new JSONObject();
		database.put("ramdb", txpdb.getRamSize());
		database.put("sqldb", txpdb.getSqlSize());
//		database.put("sqldbfile", txpdb.getSqlFile().getAbsolutePath());
		database.put("sqldbsize", MiniFormat.formatSize(txpdb.getSqlFile().length()));
		database.put("syncdb", arch.getSize());
//		database.put("syncdbfile", arch.getSQLFile().getAbsolutePath());
		database.put("syncdbsize", MiniFormat.formatSize(arch.getSQLFile().length()));
		details.put("database", database);
		
		//The main Chain
		TxPowTree txptree = MinimaDB.getDB().getTxPoWTree();
		JSONObject tree = new JSONObject();
		if(txptree.getRoot() != null) {
			tree.put("root", txptree.getRoot().getTxPoW().getTxPoWID());
			tree.put("rootblock", txptree.getRoot().getTxPoW().getBlockNumber());
			tree.put("top", txptree.getTip().getTxPoW().getTxPoWID());
			tree.put("topblock", txptree.getTip().getTxPoW().getBlockNumber().getAsLong());
			
			//Speed..
			tree.put("speed", TxPoWGenerator.getChainSpeed(txptree.getTip()));
			tree.put("difficulty", txptree.getTip().getTxPoW().getBlockDifficulty().to0xString());
			
			//Total weight..
			BigDecimal weight = MinimaDB.getDB().getCascade().getTotalWeight().add(txptree.getRoot().getTotalWeight());
			tree.put("weight", weight);
			
		}else {
			tree.put("root", "0x00");
			tree.put("tip", "0x00");
			tree.put("topblock", "-1");
		}
		tree.put("length", txptree.getSize());
		details.put("chain", tree);
		
		//The Cascade
		//..
		
		//Network..
		NetworkManager netmanager = Main.getInstance().getNetworkManager();
		details.put("network", netmanager.getStatus());
		
		//RAM usage
		long mem 		= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		String memused 	= MiniFormat.formatSize(mem);
		details.put("ram", memused);
		
		//Add all the details
		ret.put("response", details);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new status();
	}

}
