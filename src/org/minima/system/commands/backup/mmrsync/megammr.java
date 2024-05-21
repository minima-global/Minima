package org.minima.system.commands.backup.mmrsync;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
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
import org.minima.objects.base.MiniData;
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
			
			//Get all your coin proofs..
			MinimaLogger.log("Get all your CoinProofs");
			MegaMMRSyncData mydata = megammrsync.getMyDetails();
			ArrayList<CoinProof> cproofs = megammrsync.getAllCoinProofs(mydata);
			
			//Now we have the file.. lets set it..
			Main.getInstance().archiveResetReady(false);
			
			//Are we MEGA MMR
			if(GeneralParams.IS_MEGAMMR) {
				MinimaDB.getDB().getMegaMMR().clear();
			}
			
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
			
			//Import all YOUR coin proofs..
			MinimaLogger.log("Transfer your CoinProofs.. "+cproofs.size());
			for(CoinProof cp : cproofs) {
				
				//Convert to MiniData..
				MiniData cpdata = MiniData.getMiniDataVersion(cp);
				
				//Coin Import..
				JSONObject coinproofresp = CommandRunner.getRunner().runSingleCommand("coinimport track:true data:"+cpdata.to0xString());
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message", "MegaMMR import fininshed.. please restart");
			ret.put("response", resp);
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().shutdownFinalProcs();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//Now save the Mega MMR..
			File basefolder = MinimaDB.getDB().getBaseDBFolder();
	    	File mmrfile 	= new File(basefolder,"megammr.mmr");
	    	mmrback.getMegaMMR().saveMMR(mmrfile);
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
			//Tell listener..
			Main.getInstance().NotifyMainListenerOfShutDown();
		
		}else if(action.equals("check")) {
			
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
			
			//Add the cascade and tree..
			//..
			
			//Now check integrity
			Hashtable<String,Coin> allcoins = mmrback.getMegaMMR().getAllCoins();
			Collection<Coin> coincollection = allcoins.values();
			Iterator<Coin> coiniterator = coincollection.iterator();
			
			int maxcheck = 0;
			while(maxcheck < 10 &&  coiniterator.hasNext()) {
				Coin coin = coiniterator.next();
				
				//Create the MMRData Leaf Node..
				MMRData mmrdata = MMRData.CreateMMRDataLeafNode(coin, coin.getAmount());
				
				//Get the proof..
				MMRProof mmrproof = megammr.getMMR().getProof(coin.getMMREntryNumber());
				
				//Now check the proof..
				boolean valid = megammr.getMMR().checkProofTimeValid(coin.getMMREntryNumber(), mmrdata, mmrproof);
				
				MinimaLogger.log("Checking coin : "+coin.getCoinID()+" "+valid);
				
				maxcheck++;
			}
			
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new megammr();
	}

	public static void main(String[] zArgs) throws Exception {
		
		//Load it in..
		MegaMMRBackup mmrback = new MegaMMRBackup();
		
		MinimaLogger.log("Load MegaMMR..");
		MiniFile.loadObjectSlow(new File("./bin/newmega.mmr"), mmrback);
		
		//Get the mmr
		MegaMMR mega 	= mmrback.getMegaMMR();
		MMR mmr 		= mmrback.getMegaMMR().getMMR();
		
		//Load the IBD into the MMR..
		MinimaLogger.log("Add TxTree to Mega MMR..");
		ArrayList<TxBlock> blocks = mmrback.getIBD().getTxBlocks();
		for(TxBlock block : blocks) {
			//Add to the MegaMMR..
			mega.addBlock(block);
		}
		
		MinimaLogger.log("Now check all coin proofs..");
		
		
		//Add the cascade and tree..
		mmr.finalizeSet();
		
		//Now check integrity
		Hashtable<String,Coin> allcoins = mmrback.getMegaMMR().getAllCoins();
		int size = allcoins.size();
		
		Collection<Coin> coincollection = allcoins.values();
		Iterator<Coin> coiniterator = coincollection.iterator();
		
		int maxcheck = 0;
		while(coiniterator.hasNext()) {
			Coin coin = coiniterator.next();
			
			//Create the MMRData Leaf Node..
			MMRData mmrdata = MMRData.CreateMMRDataLeafNode(coin, coin.getAmount());
			
			//Get the proof..
			MMRProof mmrproof = mmr.getProof(coin.getMMREntryNumber());
			
			//Now check the proof..
			boolean valid = mmr.checkProofTimeValid(coin.getMMREntryNumber(), mmrdata, mmrproof);
			
			if(!valid) {
				throw new Exception("INVALID! @ "+coin.toJSON().toString());
			}
			
			maxcheck++;
			if(maxcheck % 5000 == 0) {
				MinimaLogger.log("Checking coins @ "+maxcheck+" / "+size);
			}
		}
		
		MinimaLogger.log("All coins checked "+maxcheck+" / "+size);
		
		
	}
}
