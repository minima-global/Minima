package org.minima.system.commands.all;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
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
import org.minima.utils.MiniFile;
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
		
		//The Database
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
		Cascade	cascade		= MinimaDB.getDB().getCascade();
		ArchiveManager arch = MinimaDB.getDB().getArchive(); 
		Wallet wallet 		= MinimaDB.getDB().getWallet();
		
		JSONObject details = new JSONObject();
		details.put("version", GlobalParams.MINIMA_VERSION);
		
		//How many Devices..
		BigDecimal blkweightdec 	= new BigDecimal(txptree.getTip().getTxPoW().getBlockDifficulty().getDataValue());
		BigDecimal blockWeight 		= Crypto.MAX_VALDEC.divide(blkweightdec, MathContext.DECIMAL32);
		
		MiniNumber ratio 			= new MiniNumber(blockWeight).div(new MiniNumber(TxPoWGenerator.MIN_HASHES));
		MiniNumber pulsespeed 		= MiniNumber.THOUSAND.div(new MiniNumber(GeneralParams.USER_PULSE_FREQ));
		
		MiniNumber usersperpulse 	= MiniNumber.ONE.div(new MiniNumber(""+pulsespeed).div(GlobalParams.MINIMA_BLOCK_SPEED));
		MiniNumber totaldevs 		= usersperpulse.mult(ratio).floor();
		
		details.put("devices", totaldevs.getAsLong());
		
		//The Current total Length of the Minima Chain
		long totallength = txptree.getHeaviestBranchLength()+cascade.getLength();
		details.put("length", totallength);
		
		//The total weight of the chain + cascade
		BigInteger chainweight 	= txptree.getRoot().getTotalWeight().toBigInteger();
		BigInteger cascweight 	= MinimaDB.getDB().getCascade().getTotalWeight().toBigInteger();
		details.put("weight", chainweight.add(cascweight));
		
		details.put("configuration", GeneralParams.CONFIGURATION_FOLDER);
		
		JSONObject files = new JSONObject();
		
		//RAM usage
		long mem 		= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		String memused 	= MiniFormat.formatSize(mem);
		files.put("ram", memused);
		
		//File Memory
		long allfiles = MiniFile.getTotalFileSize(new File(GeneralParams.CONFIGURATION_FOLDER));
		files.put("files", MiniFormat.formatSize(allfiles));
		
		JSONObject database = new JSONObject();
		database.put("txpowdb", MiniFormat.formatSize(txpdb.getSqlFile().length()));
		database.put("archivedb", MiniFormat.formatSize(arch.getSQLFile().length()));
		
		long cascsize = MinimaDB.getDB().getCascadeFileSize();
		database.put("cascade", MiniFormat.formatSize(cascsize));
		
		database.put("wallet", MiniFormat.formatSize(wallet.getSQLFile().length()));
		files.put("database", database);
		
		details.put("memory", files);
		
		//The main Chain
		JSONObject tree = new JSONObject();
		if(txptree.getRoot() != null) {
			tree.put("block", txptree.getTip().getTxPoW().getBlockNumber().getAsLong());
			tree.put("time", new Date(txptree.getTip().getTxPoW().getTimeMilli().getAsLong()).toString());
			tree.put("hash", txptree.getTip().getTxPoW().getTxPoWID());
			
//			tree.put("root", txptree.getRoot().getTxPoW().getTxPoWID());
//			tree.put("rootblock", txptree.getRoot().getTxPoW().getBlockNumber());
			
			//Speed..
			if(txptree.getTip().getTxPoW().getBlockNumber().isLessEqual(MiniNumber.TWO)){
				tree.put("speed", "1");
			}else {
				MiniNumber blocksback = GlobalParams.MINIMA_BLOCKS_SPEED_CALC;
				if(txptree.getTip().getTxPoW().getBlockNumber().isLessEqual(GlobalParams.MINIMA_BLOCKS_SPEED_CALC)) {
					blocksback = txptree.getTip().getTxPoW().getBlockNumber().decrement();
				}
				tree.put("speed", TxPoWGenerator.getChainSpeed(txptree.getTip(),blocksback).toString());
			}
			
			tree.put("difficulty", txptree.getTip().getTxPoW().getBlockDifficulty().to0xString());
		
			tree.put("size", txptree.getSize());
			tree.put("length", txptree.getHeaviestBranchLength());
			
			//Total weight..
			BigDecimal weighttree = txptree.getRoot().getTotalWeight();
			tree.put("weight", chainweight);
			
		}else {
			tree.put("root", "0x00");
		}
		
		//The Cascade
		JSONObject casc = new JSONObject();
		casc.put("start", cascade.getTip().getTxPoW().getBlockNumber().getAsLong());
		casc.put("length", cascade.getLength());
		casc.put("weight", cascweight);
		tree.put("cascade", casc);
		
		//Add the chain details
		details.put("chain", tree);
		
		//Add detailsl about the number of TxPoW we are tracking
		database = new JSONObject();
		database.put("mempool", txpdb.getAllUnusedTxns().size());
		database.put("ramdb", txpdb.getRamSize());
		database.put("txpowdb", txpdb.getSqlSize());
		database.put("archivedb", arch.getSize());
		
		//Add ther adatabse
		details.put("txpow", database);
		
		//Network..
		NetworkManager netmanager = Main.getInstance().getNetworkManager();
		details.put("network", netmanager.getStatus());
		
		//Add all the details
		ret.put("response", details);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new status();
	}

}
