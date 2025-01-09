package org.minima.system.commands.backup.mmrsync;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MegaMMR;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MiniUtil;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class megammr extends Command {

	public megammr() {
		super("megammr","(action:) (file:) - Get Info on or Import / Export the MegaMMR data");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmegammr\n"
				+ "\n"
				+ "View information about your MegaMMR. Export and Import complete MegaMMR data.\n"
				+ "\n"
				+ "You must be running -megammr.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    info   : Shows info about your MegaMMR.\n"
				+ "    export : Export a MegaMMR data file.\n"
				+ "    import : Import a MegaMMR data file.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    Use with export and import.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "megammr\n"
				+ "\n"
				+ "megammr action:export\n"
				+ "\n"
				+ "megammr action:export file:thefile\n"
				+ "\n"
				+ "megammr action:import file:thefile\n"
				;
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","file"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		String action = getParam("action","info");
		
		MegaMMR megammr = MinimaDB.getDB().getMegaMMR();
		
		if(action.equals("info")) {
		
			JSONObject resp = new JSONObject();
			resp.put("enabled", GeneralParams.IS_MEGAMMR);
			resp.put("mmr", megammr.getMMR().toJSON(false));
			resp.put("coins", megammr.getAllCoins().size());
			
			//Put the details in the response..
			ret.put("response", resp);
		
		}else if(action.equals("export")) {
			
			if(!GeneralParams.IS_MEGAMMR) {
				throw new CommandException("MegaMMR not enabled");
			}
			
			//Get the file
			String file = getParam("file","");
			if(file.equals("")) {
				//file = "megammr-backup-"+System.currentTimeMillis()+".bak";
				file = "megammr_"+MiniUtil.DATEFORMAT.format(new Date())+".megammr";
			}
			
			//Create the file
			File backupfile = MiniFile.createBaseFile(file);
			
			//get the MMR and IBD..
			if(backupfile.exists()) {
				backupfile.delete();
			}
			backupfile.createNewFile();
			
			IBD ibd = new IBD();
			
			//Lock the DB for read access..
			MinimaDB.getDB().readLock(true);
			
			try {
				
				//Create an IBD
				ibd.createCompleteIBD();
				
				MegaMMRBackup mmrbackup = new MegaMMRBackup(megammr, ibd);
				
				//Now write to it..
				FileOutputStream fos 		= new FileOutputStream(backupfile);
				BufferedOutputStream bos 	= new BufferedOutputStream(fos, 65536);
				DataOutputStream fdos 		= new DataOutputStream(bos);
				
				//And write it..
				mmrbackup.writeDataStream(fdos);
				
				//flush
				fdos.flush();
				bos.flush();
				fos.flush();
				
				fdos.close();
				bos.close();
				fos.close();
				
			}catch(Exception exc) {
				
				//Unlock DB
				MinimaDB.getDB().readLock(false);
				
				throw new CommandException(exc.toString());
			}
			
			//Unlock DB
			MinimaDB.getDB().readLock(false);
			
			JSONObject resp = new JSONObject();
			resp.put("megammrtip", megammr.getMMR().getBlockTime());
			resp.put("ibdtip", ibd.getTreeTip());
			resp.put("backup", backupfile.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(backupfile.length()));
			
			//Put the details in the response..
			ret.put("response", resp);
		
		}else if(action.equals("import")) {
			
			if(!GeneralParams.IS_MEGAMMR) {
				throw new CommandException("MegaMMR not enabled");
			}
			
			String file = getParam("file","");
			if(file.equals("")) {
				throw new CommandException("MUST specify a file to restore from");
			}
			
			//Does it exist..
			File restorefile = MiniFile.createBaseFile(file);
			if(!restorefile.exists()) {
				throw new CommandException("Restore file doesn't exist : "+restorefile.getAbsolutePath());
			}
			
			//Load it in..
			MegaMMRBackup mmrback = new MegaMMRBackup();
			
			try {
				MinimaLogger.log("Loading MegaMMR.. size:"+MiniFormat.formatSize(restorefile.length()));
				MiniFile.loadObjectSlow(restorefile, mmrback);
			}catch(Exception exc) {
				throw new CommandException(exc.toString());
			}
			
			//Now we have the file.. lets set it..
			Main.getInstance().archiveResetReady(false);
			
			//Get ready..
			MinimaDB.getDB().getMegaMMR().clear();
			
			//Now load the Mega MMR so is the current one..
			MinimaDB.getDB().hardSetMegaMMR(mmrback.getMegaMMR());
			
			//Now process the IBD.. Override the restore setting
			MinimaLogger.log("Process new IBD");
			Main.getInstance().getTxPoWProcessor().postProcessIBD(mmrback.getIBD(), "0x00", true);
			
			//Small Pause..
			while(true) {
				Thread.sleep(250);
				
				//Check
				if(Main.getInstance().getTxPoWProcessor().isIBDProcessFinished()) {
					break;
				}
			}
			
			//Quick clean
			MinimaLogger.log("System memory clean..");
			System.gc();
			
			//Get the tree
			TxPowTree tree = MinimaDB.getDB().getTxPoWTree();
			TxPoW topblock = tree.getTip().getTxPoW();
			MinimaLogger.log("Current Top Block : "+topblock.getBlockNumber());
			
			//Now check..
			TxPoW rootblock = tree.getRoot().getTxPoW();
			MinimaLogger.log("Current Tree Root : "+rootblock.getBlockNumber());
			
			//And the Mega MMR
			MegaMMR currentmega = MinimaDB.getDB().getMegaMMR();
			MinimaLogger.log("Current MegaMMR Tip : "+currentmega.getMMR().getBlockTime());
			
			//Get all your coin proofs..
			MinimaLogger.log("Get all your CoinProofs");
			MegaMMRSyncData mydata 		 = megammrsync.getMyDetails();
			ArrayList<CoinProof> cproofs = megammrsync.getAllCoinProofs(mydata);
			
			//Import all YOUR coin proofs..
			MinimaLogger.log("Transfer your CoinProofs.. "+cproofs.size());
			for(CoinProof cp : cproofs) {
				
				//Convert to MiniData..
				MiniData cpdata = MiniData.getMiniDataVersion(cp);
				
				//Coin Import..
				JSONObject coinproofresp = CommandRunner.getRunner().runSingleCommand("coinimport track:true data:"+cpdata.to0xString());
				
//				if(!(boolean)coinproofresp.get("status")) {
//					MinimaLogger.log("Fail Import : "+coinproofresp.getString("error")+" @ "+cp.toJSON());
//				}
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message", "MegaMMR import finished.. please restart");
			ret.put("response", resp);
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().shutdownFinalProcs();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
			//Tell listener..
			Main.getInstance().NotifyMainListenerOfShutDown();
		
		}else if(action.equals("integrity")) {
			
			String file = getParam("file");
			
			//Does it exist..
			File restorefile = MiniFile.createBaseFile(file);
			if(!restorefile.exists()) {
				throw new CommandException("MegaMMR file doesn't exist : "+restorefile.getAbsolutePath());
			}
			
			//Load it in..
			MegaMMRBackup mmrback = new MegaMMRBackup();
			
			MinimaLogger.log("Load MegaMMR.. "+MiniFormat.formatSize(restorefile.length()));
			MiniFile.loadObjectSlow(restorefile, mmrback);
			
			BigInteger weight = checkMegaMMR(mmrback);
			
			IBD ibd 			= mmrback.getIBD();
			TxPoW cascade 		= ibd.getCascade().getTip().getTxPoW();
			MiniNumber casctip 	= cascade.getBlockNumber();
			int casclen 		= ibd.getTxBlocks().size();
			MiniNumber chaintip	= casctip.add(new MiniNumber(casclen));
			
			JSONObject resp = new JSONObject();
			resp.put("cascadetip", casctip);
			resp.put("cascadedate", new Date(cascade.getTimeMilli().getAsLong()).toString());
			resp.put("chaintip", chaintip);
			resp.put("weight", weight.toString());
			ret.put("response", resp);
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new megammr();
	}

	public static BigInteger checkMegaMMR(File zMegaMMR) throws CommandException{
		//Load it in..
		MegaMMRBackup mmrback = new MegaMMRBackup();
		
		MinimaLogger.log("Load MegaMMR.. "+MiniFormat.formatSize(zMegaMMR.length()));
		MiniFile.loadObjectSlow(zMegaMMR, mmrback);
	
		return checkMegaMMR(mmrback);
	}
	
	public static BigInteger checkMegaMMR(MegaMMRBackup mmrback) throws CommandException{
		
		//Get the mmr
		MegaMMR mega 	= mmrback.getMegaMMR();
		MMR mmr 		= mmrback.getMegaMMR().getMMR();
		
		//Check the IBD
		IBD ibd = mmrback.getIBD();
		MinimaLogger.log("Check IBD..");
		boolean validibd = ibd.checkValidData();
		if(!validibd) {
			throw new CommandException("Invalid IBD");
		}
		
		//Check start and end.. This is where the MEGA MMR finishes..
		MiniNumber lastblock = mmr.getBlockTime();
		
		//Load the IBD into the MMR..
		ArrayList<TxBlock> blocks = mmrback.getIBD().getTxBlocks();
		for(TxBlock block : blocks) {
			
			//Check is the next in line.. 
			MiniNumber blknum = block.getTxPoW().getBlockNumber(); 
			if(!blknum.isEqual(lastblock.increment())) {
				throw new CommandException("Invalid block number.. not incremental.. last_in_mega:"+lastblock+" new_block:"+blknum);
			}
			
			//Store for later
			lastblock = blknum;
			
			//Add to the MegaMMR..
			mega.addBlock(block);
		}
		
		//You can finalize as no more being added
		mmr.finalizeSet();
		
		MinimaLogger.log("Now check all coin proofs..");
		
		//Now check integrity
		Hashtable<String,Coin> allcoins = mmrback.getMegaMMR().getAllCoins();
		int size = allcoins.size();
		
		Collection<Coin> coincollection = allcoins.values();
		Iterator<Coin> coiniterator = coincollection.iterator();
		
		int maxcheck = 0;
		while(coiniterator.hasNext()) {
			Coin coin = coiniterator.next();
			
			//Create the MMRData Leaf Node..
			MMRData mmrdata 	= MMRData.CreateMMRDataLeafNode(coin, coin.getAmount());
			MMRProof mmrproof 	= null;
			try {
				
				//Get the proof..
				mmrproof = mmr.getProof(coin.getMMREntryNumber());
			
			}catch(Exception exc) {
				throw new CommandException("Error chcking coin @ "+coin.toJSON()+" "+exc);
			}
			
			//Now check the proof..
			boolean valid = mmr.checkProofTimeValid(coin.getMMREntryNumber(), mmrdata, mmrproof);
			
			if(!valid) {
				throw new CommandException("INVALID Coin proof! @ "+coin.toJSON().toString());
			}
			
			maxcheck++;
			if(maxcheck % 5000 == 0) {
				MinimaLogger.log("Checking coins @ "+maxcheck+" / "+size);
			}
		}
		
		MinimaLogger.log("All coins checked "+maxcheck+" / "+size);
		
		return ibd.getTotalWeight();
	}
	
	public static void main(String[] zArgs) throws Exception {
		checkMegaMMR(new File("./bin/minima_megammr.mmr"));
	}
}
