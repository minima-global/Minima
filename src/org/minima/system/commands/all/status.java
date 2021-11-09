package org.minima.system.commands.all;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.wallet.Wallet;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.network.NetworkManager;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONObject;

public class status extends Command {

	public status() {
		super("status","(clean:true) - Show general status for Minima and Garbage collect RAM");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Are we clearing memory
		if(existsParam("clean")){
			System.gc();
		}
		
		JSONObject details = new JSONObject();
		details.put("version", GlobalParams.MINIMA_VERSION);
		details.put("configuration", GeneralParams.CONFIGURATION_FOLDER);
		
		//The Database
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
		Cascade	cascade		= MinimaDB.getDB().getCascade();
		ArchiveManager arch = MinimaDB.getDB().getArchive(); 
		Wallet wallet 		= MinimaDB.getDB().getWallet();
		
		JSONObject database = new JSONObject();
		database.put("ramdb", txpdb.getRamSize());
		database.put("sqldb", txpdb.getSqlSize());
//		database.put("sqldbfile", txpdb.getSqlFile().getAbsolutePath());
		database.put("sqldbsize", MiniFormat.formatSize(txpdb.getSqlFile().length()));
		database.put("archivedb", arch.getSize());
//		database.put("syncdbfile", arch.getSQLFile().getAbsolutePath());
		database.put("archivedbsize", MiniFormat.formatSize(arch.getSQLFile().length()));
		
		long cascsize = MinimaDB.getDB().getCascadeFileSize();
		database.put("cascade", MiniFormat.formatSize(cascsize));
		
		database.put("wallet", MiniFormat.formatSize(wallet.getSQLFile().length()));
		
		details.put("database", database);
		
		//The main Chain
		
		JSONObject tree = new JSONObject();
		if(txptree.getRoot() != null) {
			tree.put("time", new Date(txptree.getTip().getTxPoW().getTimeMilli().getAsLong()));
			
			tree.put("root", txptree.getRoot().getTxPoW().getTxPoWID());
			tree.put("rootblock", txptree.getRoot().getTxPoW().getBlockNumber());
			tree.put("top", txptree.getTip().getTxPoW().getTxPoWID());
			tree.put("topblock", txptree.getTip().getTxPoW().getBlockNumber().getAsLong());
			
			//Speed..
			if(txptree.getTip().getTxPoW().getBlockNumber().isLessEqual(MiniNumber.TWO)){
				tree.put("speed", 1);
			}else {
				MiniNumber blocksback = GlobalParams.MINIMA_BLOCKS_SPEED_CALC;
				if(txptree.getTip().getTxPoW().getBlockNumber().isLessEqual(GlobalParams.MINIMA_BLOCKS_SPEED_CALC)) {
					blocksback = txptree.getTip().getTxPoW().getBlockNumber().decrement();
				}
				tree.put("speed", TxPoWGenerator.getChainSpeed(txptree.getTip(),blocksback));
			}
			
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
		JSONObject casc = new JSONObject();
		casc.put("length", cascade.getLength());
		casc.put("weight", cascade.getTotalWeight().toPlainString());
		details.put("cascade", casc);
		
		//Network..
		NetworkManager netmanager = Main.getInstance().getNetworkManager();
		details.put("network", netmanager.getStatus());
		
		//RAM usage
		long mem 		= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		String memused 	= MiniFormat.formatSize(mem);
		details.put("ram", memused);
		
		//How many Devices..
		BigDecimal blkweightdec 	= new BigDecimal(txptree.getTip().getTxPoW().getBlockDifficulty().getDataValue());
		BigDecimal blockWeight 		= Crypto.MAX_VALDEC.divide(blkweightdec, MathContext.DECIMAL32);
		
		MiniNumber ratio 			= new MiniNumber(blockWeight).div(new MiniNumber(TxPoWGenerator.MIN_HASHES));
		MiniNumber pulsespeed 		= MiniNumber.THOUSAND.div(new MiniNumber(GeneralParams.USER_PULSE_FREQ));
		
		MiniNumber usersperpulse 	= MiniNumber.ONE.div(new MiniNumber(""+pulsespeed).div(GlobalParams.MINIMA_BLOCK_SPEED));
		MiniNumber totaldevs 		= usersperpulse.mult(ratio).floor();
		
		details.put("devices", totaldevs.toString());
		
		//Add all the details
		ret.put("response", details);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new status();
	}

}
