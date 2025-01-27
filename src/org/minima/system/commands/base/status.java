package org.minima.system.commands.base;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Magic;
import org.minima.objects.base.MiniData;
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
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class status extends Command {

	public status() {
		super("status","(clean:true) - Show general status for Minima and clean RAM");
	}
	
	@Override
	public String getFullHelp() {
		return "\nstatus\n"
				+ "\n"
				+ "Show the general status for Minima and your node. Optionally clean the RAM.\n"
				+ "\n"
				+ "Prints details for general status, memory used, chain info, stored txpow units, network connections, p2p connections and traffic.\n"
				+ "\n"
				+ "clean: (optional)\n"
				+ "    true only, clear the RAM.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "status\n"
				+ "\n"
				+ "status clean:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"clean","debug","complete"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Are we clearing memory
		if(existsParam("clean")){
			System.gc();
		}

		//Are we verbose output
		boolean debug 		= getBooleanParam("debug",false);
		boolean complete 	= getBooleanParam("complete",false);
		
		//The Database
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
		Cascade	cascade		= MinimaDB.getDB().getCascade();
		ArchiveManager arch = MinimaDB.getDB().getArchive(); 
		Wallet wallet 		= MinimaDB.getDB().getWallet();

//		//Do we haver any blocks..
//		if(txptree.getTip() == null) {
//			throw new CommandException("No Blocks yet..");
//		}

		JSONObject details = new JSONObject();
		details.put("version", GlobalParams.getFullMicroVersion());

		//Uptime..
		details.put("uptime", MiniFormat.ConvertMilliToTime(Main.getInstance().getUptimeMilli()));
		
		//Is the Wallet Locked..
		details.put("locked", !MinimaDB.getDB().getWallet().isBaseSeedAvailable());
		
		//How many Devices..
		if(complete) {
			
			//Are we normal Mine mode.. UNplugged on Phone ?
			boolean normalmode = Main.getInstance().isNormalMineMode();
			if(normalmode) {
				details.put("powmode", "normal");
			}else {
				details.put("powmode", "low");
			}
			
			BigDecimal blkweightdec 	= new BigDecimal(txptree.getTip().getTxPoW().getBlockDifficulty().getDataValue());
			BigDecimal blockWeight 		= Crypto.MAX_VALDEC.divide(blkweightdec, MathContext.DECIMAL32);
	
			//What is the user hashrate..
			MiniNumber userhashrate 	= MinimaDB.getDB().getUserDB().getHashRate();
			if(userhashrate.isLess(Magic.MIN_HASHES)) {
				userhashrate = Magic.MIN_HASHES;
			}
			MiniNumber ratio 			= new MiniNumber(blockWeight).div(userhashrate);
			
			//Are we in Normal mode..
			if(!Main.getInstance().isNormalMineMode()) {
				ratio = ratio.mult(MiniNumber.TEN);
			}
			
			details.put("devices", ratio.ceil().toString());
		}

		//The Current total Length of the Minima Chain
		BigInteger chainweight 	= BigInteger.ZERO;
		BigInteger cascweight 	= BigInteger.ZERO;
		if(txptree.getTip() != null) {
			long totallength = txptree.getHeaviestBranchLength()+cascade.getLength();
			details.put("length", totallength);
	
			//The total weight of the chain + cascade
			chainweight = txptree.getRoot().getTotalWeight().toBigInteger();
			cascweight 	= MinimaDB.getDB().getCascade().getTotalWeight().toBigInteger();
	
			details.put("weight", chainweight.add(cascweight).toString());
			
			//Total Minima..
			MiniNumber minima = MinimaDB.getDB().getTxPoWTree().getTip().getTxPoW().getMMRTotal();
			details.put("minima", minima.toString());
			
			//How many coins..
			BigDecimal coins = MinimaDB.getDB().getTxPoWTree().getTip().getMMR().getEntryNumber().getBigDecimal();
			details.put("coins", coins.toString());
		}else {
			details.put("length", 0);
			details.put("weight", "0");
			details.put("minima", "0");
			details.put("coins", "0");
		}
		
		details.put("data", GeneralParams.DATA_FOLDER);

		if(debug) {
			MinimaLogger.log("Main Settings Done..");
		}
		
		JSONObject files = new JSONObject();

		//RAM usage
		long mem 		= Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		String memused 	= MiniFormat.formatSize(mem);
		files.put("ram", memused);

		//File Memory
		long allfiles = MiniFile.getTotalFileSize(new File(GeneralParams.DATA_FOLDER));
		files.put("disk", MiniFormat.formatSize(allfiles));

		JSONObject database = new JSONObject();
		database.put("txpowdb", MiniFormat.formatSize(txpdb.getSqlFile().length()));
		database.put("archivedb", MiniFormat.formatSize(arch.getSQLFile().length()));
		database.put("cascade", MiniFormat.formatSize(MinimaDB.getDB().getCascadeFileSize()));
		database.put("chaintree", MiniFormat.formatSize(MinimaDB.getDB().getTxPowTreeFileSize()));
		database.put("wallet", MiniFormat.formatSize(wallet.getSQLFile().length()));
		database.put("userdb", MiniFormat.formatSize(MinimaDB.getDB().getUserDBFileSize()));
		database.put("p2pdb", MiniFormat.formatSize(MinimaDB.getDB().getP2PFileSize()));
		
		if(complete) {

			long mdsfiles = MiniFile.getTotalFileSize(Main.getInstance().getMDSManager().getRootMDSFolder());
			database.put("mds", MiniFormat.formatSize(mdsfiles));
			
			//Get ALKL the files
			JSONObject allthefiles = new JSONObject();
			MiniFile.getTotalFileSizeWithNames(new File(GeneralParams.DATA_FOLDER), allthefiles,3,0);
			database.put("allfiles", allthefiles);
		}
		
		files.put("files", database);

		details.put("memory", files);
		
		if(debug) {
			MinimaLogger.log("Memory Done..");
		}
		
		//The main Chain
		JSONObject tree = new JSONObject();
		if(txptree.getRoot() != null) {
			tree.put("block", txptree.getTip().getTxPoW().getBlockNumber().getAsLong());
			tree.put("time", new Date(txptree.getTip().getTxPoW().getTimeMilli().getAsLong()).toString());
			tree.put("hash", txptree.getTip().getTxPoW().getTxPoWID());
			
			//Speed..
			if(txptree.getTip().getTxPoW().getBlockNumber().isLessEqual(MiniNumber.TWO)){
				tree.put("speed", 1);
			}else {
				
				//What are the start and end point BEFORE we do the median..
				TxPoWTreeNode treestartblock 	= txptree.getTip();
				TxPoWTreeNode treeendblock 		= treestartblock.getParent(GlobalParams.MINIMA_BLOCKS_SPEED_CALC.getAsInt());
				
				//Now use the Median Times..
				TxPoWTreeNode startblock 	= TxPoWGenerator.getMedianTimeBlock(treestartblock);
				TxPoWTreeNode endblock 	 	= TxPoWGenerator.getMedianTimeBlock(treeendblock);
				
				MiniNumber blockdiff 		= startblock.getBlockNumber().sub(endblock.getBlockNumber()); 
				if(blockdiff.isEqual(MiniNumber.ZERO)) {
					//throw new CommandException("ZERO blockdiff on speed check.. start:"+startblock.getBlockNumber()+" end:"+endblock.getBlockNumber());
					tree.put("speed", MiniNumber.MINUSONE);
				}else {
					MiniNumber speed = TxPoWGenerator.getChainSpeed(startblock, blockdiff);
					tree.put("speed", speed.setSignificantDigits(5));
				}
			}
			
			MiniData difficulty = new MiniData(txptree.getTip().getTxPoW().getBlockDifficulty().getBytes(),32);
			tree.put("difficulty", difficulty.to0xString());

			tree.put("size", txptree.getSize());
			tree.put("length", txptree.getHeaviestBranchLength());
			tree.put("branches", txptree.getSize() - txptree.getHeaviestBranchLength());
			
			//Total weight..
			BigDecimal weighttree = txptree.getRoot().getTotalWeight();
			tree.put("weight", chainweight.toString());
			
		}else {
			tree.put("block", 0);
			tree.put("time", "NO BLOCKS YET");
			tree.put("hash", "0x00");
			tree.put("length", 0);
			tree.put("branches", 0);
		}
		
		if(debug) {
			MinimaLogger.log("Chain Tree done..");
		}
		
		//The Cascade
		JSONObject casc = new JSONObject();
		if(cascade.getTip() != null) {
			casc.put("start", cascade.getTip().getTxPoW().getBlockNumber().getAsLong());
		}else {
			casc.put("start", -1);
		}
		casc.put("length", cascade.getLength());
		casc.put("weight", cascweight.toString());
		tree.put("cascade", casc);

		//Add the chain details
		details.put("chain", tree);

		//Add detailsl about the number of TxPoW we are tracking
		database = new JSONObject();
		database.put("mempool", txpdb.getAllUnusedTxns().size());
		if(debug) {
			MinimaLogger.log("Mempool done..");
		}
		database.put("ramdb", txpdb.getRamSize());
		if(debug) {
			MinimaLogger.log("RamDB done..");
		}
		database.put("txpowdb", txpdb.getSqlSize());
		if(debug) {
			MinimaLogger.log("txpowdb done..");
		}
		
//		if(complete) {
//			//Archive DB data
//			JSONObject archdb 	= new JSONObject();
//			int size 			= arch.getSize();
//			Cascade archcasc 	= arch.loadCascade(); 
//			archdb.put("size", size);
//			if(size>0) {
//				archdb.put("start", arch.loadLastBlock().getTxPoW().getBlockNumber().toString());
//				archdb.put("startdate", new Date(arch.loadLastBlock().getTxPoW().getTimeMilli().getAsLong()).toString());
//				archdb.put("end", arch.loadFirstBlock().getTxPoW().getBlockNumber().toString());
//				if(archcasc!=null) {
//					archdb.put("cascadetip", archcasc.getTip().getTxPoW().getBlockNumber());	
//				}
//			}
//			database.put("archivedb", archdb);
//			if(debug) {
//				MinimaLogger.log("archivedb done..");
//			}
//		}else {
			database.put("archivedb", arch.getSize());
			if(debug) {
				MinimaLogger.log("archivedb done..");
			}
//		}
		
		//Add ther adatabse
		details.put("txpow", database);
		
		if(debug) {
			MinimaLogger.log("Database done..");
		}
		
		//Network..
		NetworkManager netmanager = Main.getInstance().getNetworkManager();
		if(netmanager!=null) {
			details.put("network", netmanager.getStatus());
		}
		
		if(debug) {
			MinimaLogger.log("Network done..");
		}
		
		//Add all the details
		ret.put("response", details);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new status();
	}

	public static void main(String[] zARgs) {
		
		File ff = new File("C:\\Users\\spartacusrex\\.minima\\1.0");
		
		JSONObject files = new JSONObject();
		MiniFile.getTotalFileSizeWithNames(ff, files,3,0);
		
		System.out.println(MiniFormat.JSONPretty(files));
		
	}
	
}
