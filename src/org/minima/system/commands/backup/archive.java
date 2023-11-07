package org.minima.system.commands.backup;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Random;
import java.util.zip.GZIPInputStream;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.archive.RawArchiveInput;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWProcessor;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.network.connect;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;

public class archive extends Command {
	
	//Are we restoring from a local archive..
	private final String LOCAL_ARCHIVE 					= "archiverestore";
	
	//H2 local
	private static boolean H2_TEMPARCHIVE 				= true;
	
	private static ArchiveManager STATIC_TEMPARCHIVE 	= null;
	private static RawArchiveInput STATIC_RAW		 	= null;
	
	public archive() {
		super("archive","[action:] (host:) (phrase:) (keys:) (keyuses:) - Resync your chain with seed phrase if necessary (otherwise wallet remains the same)");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","phrase","anyphrase","keys","keyuses","file","address","statecheck"}));
	}
	
	@Override
	public String getFullHelp() {
		return "\narchive\n"
				+ "\n"
				+ "Perform a chain or seed re-sync from an archive node or archive export file.\n"
				+ "\n"
				+ "A chain re-sync will put your node on the correct chain.\n"
				+ "\n"
				+ "Use a chain re-sync if your node has been offline for too long and cannot catchup. Seed Phrase is not required.\n"
				+ "\n"
				+ "A seed re-sync will wipe the wallet, re-generate your private keys and restore your coins.\n"
				+ "\n"
				+ "Only use a seed re-sync if you have lost your node and do not have a backup.\n"
				+ "\n"
				+ "You can also perform checks on your archive db or archive file and check an address.\n"
				+ "\n"
				+ "action:\n"
				+ "    resync : do a chain or seed re-sync. For chain re-sync do not provide phrase. Use with 'host'.\n"
				+ "    integrity : Check the integrity of your archive db. No host required.\n"
				+ "    export : Export your archive db to a gzip file. \n"
				+ "    import : do a chain or seed re-sync using an archive gzip file. Use with 'file'.\n"
				+ "    inspect : inspect an archive export gzip file. If 'last:1' then the file can re-sync any node from genesis.\n"
				+ "    addresscheck : check your archive db for spent and unspent coins at a specific address.\n"
				+ "\n"
				+ "host: (optional) \n"
				+ "    ip:port of the archive node to sync from or check the integrity of.\n"
				+ "    Only use with 'action:resync'.\n"
				+ "\n"
				+ "file: (optional) \n"
				+ "    name or path of the archive export gzip file to export/import/inspect\n"
				+ "\n"
				+ "phrase: (optional)\n"
				+ "    Your 24 word seed phrase in double quotes, to perform a seed re-sync. Use with action:resync or import.\n"
				+ "    This will wipe the wallet of this node. You do NOT have to do this if you still have access to your wallet.\n"
				+ "    In this case, just do a re-sync without 'phrase' to get on the correct chain.\n"
				+ "\n"
				+ "keys: (optional) \n"
				+ "    Number of keys to create if you need to do a seed re-sync. Default is 64.\n"
				+ "\n"
				+ "keyuses: (optional) \n"
				+ "    How many times at most you used your keys..\n"
				+ "    Every time you re-sync with seed phrase this needs to be higher as Minima Signatures are stateful.\n"
				+ "    Defaults to 1000 - the max is 262144 for normal keys.\n"
				+ "\n"
				+ "address: (optional) \n"
				+ "    The wallet address to check. Use with 'action:addresscheck'.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "archive action:resync host:89.98.89.98:9001\n"
				+ "\n"
				+ "archive action:resync host:89.98.89.98:9001 phrase:\"YOUR 24 WORD SEED PHRASE\" keyuses:2000\n"
				+ "\n"
				+ "archive action:integrity\n"
				+ "\n"
				+ "archive action:export file:archiveexport-ddmmyy.gzip\n"
				+ "\n"
				+ "archive action:import file:archiveexport-ddmmyy.gzip\n"
				+ "\n"
				+ "archive action:inspect file:archiveexport-ddmmyy.gzip\n"
				+ "\n"
				+ "archive action:addresscheck address:Mx..\n"
				+ "\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String action = getParam("action");
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		if(action.equals("integrity")) {
			
			//Scan through the entire DB.. checking.. 
			MinimaLogger.log("Checking Archive DB.. this may take some time..");
			
//			JSONObject res = arch.executeGenericSQL("SELECT * FROM cascadedata");
//			MinimaLogger.log(MiniFormat.JSONPretty(res));
			
			//What is the first block in the DB
			TxBlock starterblock = arch.loadLastBlock();
			
			//What is the first entry
			boolean startcheck 	= true;
			boolean startatroot = false;
			MiniNumber lastlog 	= MiniNumber.ZERO;
			MiniNumber start 	= MiniNumber.ZERO;
			if(starterblock == null) {
				MinimaLogger.log("You have no Archive blocks..");
				startcheck = false;
			}else {
				lastlog = starterblock.getTxPoW().getBlockNumber();
				start 	= lastlog;
				
				//Is it from root..
				MiniNumber startblocknumber = starterblock.getTxPoW().getBlockNumber();
				if(startblocknumber.isEqual(MiniNumber.ONE)) {
					MinimaLogger.log("ArchiveDB starts at root");
					startatroot = true;
				}
			}
			
			//Get the cascade details
			MiniNumber cascstart 	= MiniNumber.MINUSONE;
			JSONObject cascjson 	= new JSONObject();
			Cascade dbcasc = arch.loadCascade(); 
			if(dbcasc != null) {
				cascjson.put("exists", true);
				
				//Get the tip..
				cascstart = dbcasc.getTip().getTxPoW().getBlockNumber();
				cascjson.put("tip", cascstart.toString());
				cascjson.put("length", dbcasc.getLength());
				
			}else {
				cascjson.put("exists", false);
			}
			
			//Get t the initial 1000
			MiniData parenthash 	= null;
			MiniNumber parentnum 	= null;
			int errorsfound 		= 0;
			int total 				= 0;
			MiniNumber archstart 	= start;
			MiniNumber archend 		= archstart;
			
			JSONObject archjson = new JSONObject();
			archjson.put("start", archstart.toString());
			
			while(startcheck) {
				
				//Do we log a message
				if(lastlog.isLess(start.sub(new MiniNumber(2000)))) {
					MinimaLogger.log("Now checking from  "+start);
					lastlog = start;
				}
				
				//Use batches of 256
				MiniNumber end 	= start.add(MiniNumber.TWOFIVESIX);
				
				//Get some blocks
				ArrayList<TxBlock> blocks = arch.loadBlockRange(start.decrement(),end,false); 
				
				for(TxBlock block : blocks) {
					archend = block.getTxPoW().getBlockNumber();
					total++;
					
					//Start Checking..
					if(parenthash == null) {
						parenthash 	= block.getTxPoW().getTxPoWIDData();
						parentnum  	= block.getTxPoW().getBlockNumber();
						lastlog 	= parentnum;
						
						archstart 	= parentnum;
						
						MinimaLogger.log("ArchiveDB blocks resync start at block "+parentnum+" @ "+new Date(block.getTxPoW().getTimeMilli().getAsLong()));
						
					}else {
						
						//Check correct number
						if(!block.getTxPoW().getBlockNumber().isEqual(parentnum.increment())) {
							MinimaLogger.log("Incorrect child block @ "+block.getTxPoW().getBlockNumber()+" parent:"+parentnum);
							errorsfound++;
						}else if(!block.getTxPoW().getParentID().isEqual(parenthash)) {
							MinimaLogger.log("Parent hash incorrect @ "+block.getTxPoW().getBlockNumber());
							errorsfound++;
						}
						
						parenthash 	= block.getTxPoW().getTxPoWIDData();
						parentnum 	= block.getTxPoW().getBlockNumber();
					}
				}
				
				//Have we checked them all..
				if(blocks.size()==0) {
					break;
				}
				
				//Now recycle..
				start = parentnum.increment();
			}
			
			//Add more details
			archjson.put("end", archend.toString());
			archjson.put("blocks", total);
			
			//Check the archive node starts in the cascade..
			MiniNumber startresync 	= archstart;
			boolean validlist 		= false;
			if(!archstart.isEqual(MiniNumber.ONE)) {
				if(cascstart.isMoreEqual(archstart.sub(MiniNumber.ONE)) && cascstart.isLessEqual(archend)) {
					validlist = true;
				}
				
				startresync = cascstart;
			}else {
				validlist = true;
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive integrity check completed");
			resp.put("cascade", cascjson);
			resp.put("archive", archjson);
			resp.put("valid", validlist);
			if(!validlist) {
				resp.put("notvalid", "Your cascade and blocks do not line up.. new cascade required.. pls restart Minima");
			}
			resp.put("from", startresync);
			resp.put("errors", errorsfound);
			
			if(errorsfound>0) {
				resp.put("recommend", "There are errors in your Archive DB blocks - you should wipe then resync with a valid host");
			}else {
				resp.put("recommend", "Your ArchiveDB is correct and has no errors.");
			}
			
			ret.put("response", resp);
			
		}else if(action.equals("resync")) {
			
			//Can only do this if all keys created..
			vault.checkAllKeysCreated();
			
			//Get the Minima Listener..
			MessageListener minimalistener = Main.getInstance().getMinimaListener();
			
			//Get the host
			String fullhost = getParam("host");
			
			//Is it auto
			boolean usinglocal = false;
			if(fullhost.equals(LOCAL_ARCHIVE)) {
				
				//Using the local DB..
				if(H2_TEMPARCHIVE && STATIC_TEMPARCHIVE == null) {
					throw new CommandException("No Local STATIC Archive DB Found..");
				}
				
				usinglocal = true;
			}
			
			Message connectdata = connect.createConnectMessage(fullhost);
			
			String host = null;
			int port 	= 0;
			if(!usinglocal) {
				
				//Check a valid host
				if(connectdata == null) {
					throw new CommandException("Invalid HOST format for resync : "+fullhost);
				}
				
				host = connectdata.getString("host");
				port = connectdata.getInteger("port");
			}
			
			//How many Keys do we need to generate
			int keys = getNumberParam("keys", new MiniNumber(Wallet.NUMBER_GETADDRESS_KEYS)).getAsInt();
			
			//Set the key uses to this..
			int keyuses = getNumberParam("keyuses", new MiniNumber(1000)).getAsInt();
			
			//Before we start deleting - check connection..
			if(!usinglocal) {
				IBD ibdtest = sendArchiveReq(host, port, MiniNumber.MINUSONE);
				if(ibdtest == null) {
					throw new CommandException("Could not connect to Archive host! @ "+host+":"+port);
				}
			}
			
			//Are we resetting the wallet too ?
			MiniData seed 		= null;
			String phrase = getParam("phrase","");
			if(!phrase.equals("")) {
			
				//Are we allowing ANY phrase..
				boolean anyphrase = getBooleanParam("anyphrase", false);
				
				//Clean it up..
				String cleanphrase = phrase;
				if(!anyphrase) {
					cleanphrase = BIP39.cleanSeedPhrase(phrase);
				}
				
				//reset ALL the default data
				Main.getInstance().archiveResetReady(true);
				
				//This can take soem time..
				MinimaLogger.log("Resetting all wallet private keys..");
				
				//Convert that into a seed..
				seed = BIP39.convertStringToSeed(cleanphrase);
				
				//Get the Wallet
				Wallet wallet = MinimaDB.getDB().getWallet();
				
				//Set it..
				wallet.updateSeedRow(cleanphrase, seed.to0xString());
				
				//Now cycle through all the default wallet keys..
				MinimaLogger.log("Creating a total of "+keys+" keys / addresses..");
				for(int i=0;i<keys;i++) {
					NotifyListener(minimalistener,"Creating key "+i);
					MinimaLogger.log("Creating key "+i);
					
					//Create a new key..
					wallet.createNewSimpleAddress(true);
				}
				MinimaLogger.log("All keys created..");
				
				//Now Update the USES - since they may have been used before - we don;t know.. 
				wallet.updateAllKeyUses(keyuses);
				
			}else {
				//reset ALL the default data
				Main.getInstance().archiveResetReady(false);
			}
			
			//Now cycle through the chain..
			MiniNumber startblock 	= MiniNumber.ZERO;
			long starttime			= 0;
			MiniNumber endblock 	= MiniNumber.ZERO;
			boolean foundsome 		= false;
			boolean firstrun 		= true;
			MiniNumber firstStart   = MiniNumber.ZERO;
			
			long lastlogmessage = 0;
			
			int counter = 0;
			MinimaLogger.log("System clean..");
			System.gc();
			IBD ibd = null;
			while(true) {
				
				//We don't need any transactions in RamDB
				MinimaDB.getDB().getTxPoWDB().wipeDBRAM();
				
				//Clean system counter
				counter++;
//				if(counter % 5 == 0) {
//					long mem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
//					if(mem > 250 * 1024 * 1024) {
//						Main.getInstance().resetMemFull();
//						MinimaLogger.log("Clean up memory..");
//					}else {
//						//MinimaLogger.log("RAM memory usage still low.. wait for cleanup");
//					}
//				}
				
				//HARD RESET - H2 database doesn't like it if I don't do this
				if(counter % 10 == 0) {
					MinimaLogger.log("Clean up memory..");
					Main.getInstance().resetMemFull();
				}
				
				//Send him a message..
				if(!usinglocal) {
					ibd = sendArchiveReq(host, port, startblock);
				}else {
					
					if(H2_TEMPARCHIVE) {
						ibd = new IBD();
						ibd.createArchiveIBD(startblock, STATIC_TEMPARCHIVE, true);
					}else {
						ibd = STATIC_RAW.getNextIBD();
					}
				}
				
				//Make sure something returned..
				if(ibd == null) {
					ibd = new IBD();
				}
				
				//Is there a cascade..
				if(startblock.isEqual(MiniNumber.ZERO) && ibd.hasCascade()) {
					MinimaLogger.log("Cascade Received.. "+ibd.getCascade().getTip().getTxPoW().getBlockNumber());
					
					//Set it as our cascade
					MinimaDB.getDB().setIBDCascade(ibd.getCascade());
					
					//Do we need to save this..
					MinimaDB.getDB().getArchive().checkCascadeRequired(ibd.getCascade());
				}
				
				int size = ibd.getTxBlocks().size();
				
				if(size > 0) {
					foundsome 		= true;
					TxBlock start 	= ibd.getTxBlocks().get(0);
					if(firstrun) {
						firstrun 	= false;
						firstStart 	= start.getTxPoW().getBlockNumber();
					}
					
					TxBlock last 	= ibd.getTxBlocks().get(size-1);
					endblock		= last.getTxPoW().getBlockNumber();
					
					startblock 		= endblock.increment();
					starttime		= last.getTxPoW().getTimeMilli().getAsLong();
					
					//MinimaLogger.log("Archive IBD received start : "+start.getTxPoW().getBlockNumber()+" end : "+endblock);
				
					//Notify the Android Listener
					NotifyListener(minimalistener,"Loading "+start.getTxPoW().getBlockNumber()+" @ "+new Date(start.getTxPoW().getTimeMilli().getAsLong()).toString());
				}else {
					MinimaLogger.log("No Archive TxBlocks left..");
				}
			
				//Post it..
				Main.getInstance().getTxPoWProcessor().postProcessArchiveIBD(ibd, "0x00");
			
				//Now wait for something to happen
				boolean error = false;
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				int attempts = 0;
				while(foundsome && tip == null) {
					Thread.sleep(250);
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
					attempts++;
					if(attempts>5000) {
						error = true;
						break;
					}
				}
				
				if(error) {
					MinimaLogger.log("ERROR : There was an error processing that FIRST IBD");
					break;
				}
				
				//Now wait to catch up..
				long timenow = System.currentTimeMillis();
				
				//MinimaLogger.log("Waiting for chain to catch up.. please wait");
				attempts = 0;
				while(foundsome) {
					if(!tip.getBlockNumber().isEqual(endblock)) {
						Thread.sleep(50);
					}else {
						break;
					}
					
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
					
					attempts++;
					if(attempts>20000) {
						error = true;
						break;
					}
				}
				
				//Do we print a log..
				if((System.currentTimeMillis() - lastlogmessage)>5000) {
					MinimaLogger.log("IBD Processed.. block:"+startblock+" @ "+MinimaLogger.DATEFORMAT.format(new Date(starttime)));
					lastlogmessage = System.currentTimeMillis();
				}
				
				if(error) {
					MinimaLogger.log("ERROR : There was an error processing that IBD - took too long");
					break;
				}
				
				//Do we have enough to ask again.. 
				if(size==0) {
					break;
				}
				
				//HACK
				//if(startblock.isMore(new MiniNumber(100000))) {
				//	MinimaLogger.log("FORCE ARCHIVE STOP @ 100000");
				//	break;
				//}
			}
			
			//Notify the Android Listener
			NotifyListener(minimalistener,"All blocks loaded.. pls wait");
			MinimaLogger.log("All Archive data received and processed.. shutting down.."); 
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive sync completed.. shutting down now.. please restart after");
			resp.put("start", firstStart.toString());
			resp.put("end", endblock.toString());
			ret.put("response", resp);
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().shutdownFinalProcs();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
			//Tell the listener
			NotifyListener(minimalistener,"SHUTDOWN");
			
			//Tell listener..
			Main.getInstance().NotifyMainListenerOfShutDown();
		
		}else if(action.equals("export")) {
			
			//The GZIPPED file 
			String file = getParam("file","archivebackup-"+System.currentTimeMillis()+".gzip");
			
			//Create the file
			File gzoutput = MiniFile.createBaseFile(file);
			if(gzoutput.exists()) {
				gzoutput.delete();
			}
			
			//Write out in GZIP format
			MinimaLogger.log("Exporting ArchiveDB to GZIPPED SQL..");
			MinimaDB.getDB().getArchive().backupToFile(gzoutput,true);
			
			long gziplen = gzoutput.length();
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive DB GZIPPED");
			resp.put("rows", MinimaDB.getDB().getArchive().getSize());
			resp.put("file", gzoutput.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(gziplen));
			ret.put("response", resp);
		
		}else if(action.equals("importold")) {
			
			//Get the file
			String file = getParam("file");
			
			//Does it exist..
			File restorefile = MiniFile.createBaseFile(file);
			if(!restorefile.exists()) {
				throw new Exception("Restore file doesn't exist : "+restorefile.getAbsolutePath());
			}
			
			//And now restore
			ArchiveManager archtemp = new ArchiveManager();
			
			//Create a temp DB file..
			File restorefolder = new File(GeneralParams.DATA_FOLDER,"archiverestore");
			restorefolder.mkdirs();
			
			File tempdb = new File(restorefolder,"archivetemp");
			if(tempdb.exists()) {
				tempdb.delete();
			}
			archtemp.loadDB(tempdb);
			
			//Restore from File..
			MinimaLogger.log("Restoring ArchiveDB from file..");
			archtemp.restoreFromFile(restorefile,true);
			
			Cascade casc = archtemp.loadCascade();
			if(casc != null) {
				MinimaLogger.log("Archive DB cascade start : "+casc.getTip().getTxPoW().getBlockNumber()+" length:"+casc.getLength());
			}
			
			TxBlock first 	= archtemp.loadFirstBlock();
			if(first!=null) {
				MinimaLogger.log("Archive DB first block : "+first.getTxPoW().getBlockNumber());
			}
			TxBlock last 	= archtemp.loadLastBlock();
			if(last!=null) {
				MinimaLogger.log("Archive DB last block : "+last.getTxPoW().getBlockNumber());
			}
			
			//Set this statically..
			STATIC_TEMPARCHIVE = archtemp;
			
			//Now run a chain sync.. with correct params
			String command = "archive action:resync host:"+LOCAL_ARCHIVE;
			if(existsParam("phrase")) {
				command = command+" phrase:\""+getParam("phrase")+"\"";
			}
			
			if(existsParam("keys")) {
				command = command+" keys:"+getParam("keys");
			}
			
			if(existsParam("keyuses")) {
				command = command+" keyuses:"+getParam("keyuses");
			}
			
			JSONArray res 		= Command.runMultiCommand(command);
			JSONObject result 	= (JSONObject) res.get(0);
			
			//Shutdwon TEMP DB
			archtemp.saveDB(false);
			
			//Delete the restore folder
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
			
			//Reset 
			STATIC_TEMPARCHIVE = null;
			
			JSONObject resp = new JSONObject();
			resp.put("archiveresync", result);
			ret.put("response", resp);
				
		}else if(action.equals("import")) {
			
			//Get the file
			String file = getParam("file");
			
			//Does it exist..
			File restorefile = MiniFile.createBaseFile(file);
			if(!restorefile.exists()) {
				throw new Exception("Restore file doesn't exist : "+restorefile.getAbsolutePath());
			}
			
			//Is it an H2 gzip or a raw dat
			boolean h2import = true; 
			if(file.endsWith(".dat")) {
				h2import = false;
			}
			
			if(h2import) {
				MinimaLogger.log("H2 archive imprt started..");
				
				//And now restore
				ArchiveManager archtemp = new ArchiveManager();
				H2_TEMPARCHIVE			= true;
				
				//Create a temp DB file..
				File restorefolder = new File(GeneralParams.DATA_FOLDER,"archiverestore");
				restorefolder.mkdirs();
				
				File tempdb = new File(restorefolder,"archivetemp");
				if(tempdb.exists()) {
					tempdb.delete();
				}
				archtemp.loadDB(tempdb);
				
				//Restore from File..
				MinimaLogger.log("Restoring ArchiveDB from file..");
				archtemp.restoreFromFile(restorefile,true);
				
				Cascade casc = archtemp.loadCascade();
				if(casc != null) {
					MinimaLogger.log("Archive DB cascade start : "+casc.getTip().getTxPoW().getBlockNumber()+" length:"+casc.getLength());
				}
				
				TxBlock first 	= archtemp.loadFirstBlock();
				if(first!=null) {
					MinimaLogger.log("Archive DB first block : "+first.getTxPoW().getBlockNumber());
				}
				TxBlock last 	= archtemp.loadLastBlock();
				if(last!=null) {
					MinimaLogger.log("Archive DB last block : "+last.getTxPoW().getBlockNumber());
				}
				
				//Set this statically..
				STATIC_TEMPARCHIVE = archtemp;
			
				//Now run a chain sync.. with correct params
				String command = "archive action:resync host:"+LOCAL_ARCHIVE;
				if(existsParam("phrase")) {
					command = command+" phrase:\""+getParam("phrase")+"\"";
				}
				
				if(existsParam("keys")) {
					command = command+" keys:"+getParam("keys");
				}
				
				if(existsParam("keyuses")) {
					command = command+" keyuses:"+getParam("keyuses");
				}
				
				JSONArray res 		= Command.runMultiCommand(command);
				JSONObject result 	= (JSONObject) res.get(0);
				
				//Shutdown TEMP DB
				archtemp.saveDB(false);
				
				//Delete the restore folder
				MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
				
				//Reset 
				STATIC_TEMPARCHIVE = null;
				
				JSONObject resp = new JSONObject();
				resp.put("archiveresync", result);
				ret.put("response", resp);
			
			}else {
				MinimaLogger.log("RAW archive imprt started..");
				
				//RAW import..
				H2_TEMPARCHIVE	= false;
				
				//Set this statically..
				STATIC_RAW = new RawArchiveInput(restorefile);
				STATIC_RAW.connect();
				
				//Now run a chain sync.. with correct params
				String command = "archive action:resync host:"+LOCAL_ARCHIVE;
				if(existsParam("phrase")) {
					command = command+" phrase:\""+getParam("phrase")+"\"";
				}
				
				if(existsParam("keys")) {
					command = command+" keys:"+getParam("keys");
				}
				
				if(existsParam("keyuses")) {
					command = command+" keyuses:"+getParam("keyuses");
				}
				
				JSONArray res 		= Command.runMultiCommand(command);
				JSONObject result 	= (JSONObject) res.get(0);
				
				//Shutdown TEMP DB
				STATIC_RAW.stop();
				
				//Reset 
				STATIC_RAW = null;
				
				JSONObject resp = new JSONObject();
				resp.put("archiveresync", result);
				ret.put("response", resp);
			}
			
		}else if(action.equals("dataimport")) {
			
			//Get the file
			String file = getParam("file");
			
			//Does it exist..
			File restorefile = MiniFile.createBaseFile(file);
			if(!restorefile.exists()) {
				throw new Exception("Restore file doesn't exist : "+restorefile.getAbsolutePath());
			}
			
			//reset ALL the default data
			Main.getInstance().archiveResetReady(false);
			
			RawArchiveInput rawin = new RawArchiveInput(restorefile);
			rawin.connect();
			
			int ibdcount = 0;
			while(true) {
				
				//Get the next IBD
				IBD syncibd = rawin.getNextIBD();
				
				//Are there any blocks..
				int size = syncibd.getTxBlocks().size();
				if(size==0) {
					break;
				}
				
				//Get the main processor
				TxPoWProcessor proc = Main.getInstance().getTxPoWProcessor();
				proc.postProcessArchiveIBD(syncibd, "0x00");
				ibdcount++;
				
				//Do this every 10 IBD..
				if(ibdcount % 10 == 0) {
					
					//Last block
					TxBlock block = syncibd.getTxBlocks().get(size-1);
					
					//Now wait for something to happen
					TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
					while(tip == null) {
						Thread.sleep(50);
						tip = MinimaDB.getDB().getTxPoWTree().getTip();
					}
					
					long starttime = block.getTxPoW().getTimeMilli().getAsLong();
					MinimaLogger.log("Processing block:"+block.getTxPoW().getBlockNumber()+" @ "+MinimaLogger.DATEFORMAT.format(new Date(starttime)));
					
					while(!tip.getBlockNumber().isEqual(block.getTxPoW().getBlockNumber())) {
						Thread.sleep(50);
						tip = MinimaDB.getDB().getTxPoWTree().getTip();
					}
					
					//Clean up..
					Main.getInstance().resetMemFull();
				}
			}
			
			//Close it down..
			rawin.stop();
			
			MinimaLogger.log("All blocks processed..");
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive sync completed.. shutting down now.. please restart after");
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
			
		}else if(action.equals("inspect")) {
			
			//Get the file
			String file = getParam("file","");
			
			//Where the temp db will go..
			File restorefolder 	= new File(GeneralParams.DATA_FOLDER,"archiverestore");
			
			//Delete the restore folder - just in case is already there
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
			
			//Do we load one..
			ArchiveManager archtemp = null;
			if(!file.equals("")) {
				//Does it exist..
				File restorefile = MiniFile.createBaseFile(file);
				if(!restorefile.exists()) {
					throw new Exception("Restore file doesn't exist : "+restorefile.getAbsolutePath());
				}
				
				//And now restore
				archtemp = new ArchiveManager();
				
				//Create a temp DB file..
				restorefolder.mkdirs();
				
				File tempdb = new File(restorefolder,"archivetemp");
				if(tempdb.exists()) {
					tempdb.delete();
				}
				archtemp.loadDB(tempdb);
				
				MinimaLogger.log("Restoring ArchiveDB from file..");
				archtemp.restoreFromFile(restorefile,true);
			}else {
				archtemp = MinimaDB.getDB().getArchive();
			}
			
			//Inspect File..
			JSONObject resp 	= new JSONObject();
			JSONObject jcasc 	= new JSONObject();
			JSONObject jarch 	= new JSONObject();
			
			resp.put("cascade", jcasc);
			resp.put("archive", jarch);
			
			jcasc.put("exists", false);
			jcasc.put("start", "-1");
			jcasc.put("length", "-1");
			jarch.put("first", "-1");
			jarch.put("last", "-1");
			jarch.put("size", "-1");
			
			Cascade casc = archtemp.loadCascade();
			if(casc != null) {
				jcasc.put("exists", true);
				jcasc.put("start", casc.getTip().getTxPoW().getBlockNumber().toString());
				jcasc.put("length", casc.getLength());
			}
			
			TxBlock first 	= archtemp.loadFirstBlock();
			if(first!=null) {
				jarch.put("first", first.getTxPoW().getBlockNumber().toString());
			}
			TxBlock last 	= archtemp.loadLastBlock();
			if(last!=null) {
				jarch.put("last", last.getTxPoW().getBlockNumber().toString());
			}
			
			//And finally the size
			jarch.put("size", archtemp.getSize());
			
			//Shutdwon TEMP DB
			if(!file.equals("")) {
				archtemp.saveDB(false);
			}
			
			//Delete the restore folder
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
		
			ret.put("response", resp);
			
		}else if(action.equals("addresscheck")) {
			
			//Which address are we looking for
			String address = getAddressParam("address");
			
			//Is there a state aswell ? 
			String statecheck = getParam("statecheck", "");
			if(	statecheck.toLowerCase().startsWith("mx") && 
				statecheck.indexOf("@")==-1) {
				//Convert to 0x format - as all statevariables are
				statecheck = Address.convertMinimaAddress(statecheck).to0xString();
			}
			
			//Cycle through
			JSONObject resp 	= new JSONObject();
			JSONArray inarr 	= new JSONArray();
			JSONArray outarr 	= new JSONArray();
			
			ArchiveManager adb 		= MinimaDB.getDB().getArchive();
			TxBlock startblock 		= adb.loadLastBlock();
			
			MiniNumber firstStart = MiniNumber.ZERO;
			boolean canstart = true;
			if(startblock == null) {
				canstart = false;
			}else {
				firstStart   = startblock.getTxPoW().getBlockNumber();
				MinimaLogger.log("Start archive @ "+firstStart);
			}
			while(canstart) {
				
				//Create an IBD for the mysql data
				ArrayList<TxBlock> mysqlblocks = adb.loadBlockRange(firstStart, firstStart.add(MiniNumber.HUNDRED), false);
				if(mysqlblocks.size()==0) {
					//No blocks left
					break;
				}
				
				for(TxBlock block : mysqlblocks) {
					
					//For the next loop
					firstStart = block.getTxPoW().getBlockNumber(); 
					
					//Get details
					TxPoW txp 			= block.getTxPoW();
					long blocknumber 	= txp.getBlockNumber().getAsLong();
					
					//Date string
					String date = MinimaLogger.DATEFORMAT.format(new Date(txp.getTimeMilli().getAsLong()));
					
					//Created
					ArrayList<Coin> outputs 		= block.getOutputCoins();
					for(Coin cc : outputs) {
						if(cc.getAddress().to0xString().equals(address)) {
							
							boolean found = true;
							if(!statecheck.equals("")) {
								//Check for state aswell..
								found = cc.checkForStateVariable(statecheck);
							}
							
							if(found) {
								MinimaLogger.log("BLOCK "+blocknumber+" CREATED COIN : "+cc.toString());
								
								JSONObject created = new JSONObject();
								created.put("block", blocknumber);
								created.put("date", date);
								created.put("datemilli", txp.getTimeMilli().toString());
								created.put("coin", cc.toJSON());
								outarr.add(created);
							}
							
						}
					}
					
					//Spent
					ArrayList<CoinProof> inputs  	= block.getInputCoinProofs();
					for(CoinProof incoin : inputs) {
						if(incoin.getCoin().getAddress().to0xString().equals(address)) {
							
							boolean found = true;
							if(!statecheck.equals("")) {
								//Check for state aswell..
								found = incoin.getCoin().checkForStateVariable(statecheck);
							}
							
							if(found) {
								MinimaLogger.log("BLOCK "+blocknumber+" SPENT COIN : "+incoin.getCoin().toString());
								
								JSONObject spent = new JSONObject();
								spent.put("block", blocknumber);
								spent.put("date", date);
								spent.put("datemilli", txp.getTimeMilli().toString());
								spent.put("coin", incoin.getCoin().toJSON());
								inarr.add(spent);
							}
						}
					}
				}
			}
			
			//And Now check the chain..
			if(startblock!=null) {
				MinimaLogger.log("End archive @ "+firstStart);
			}
			
			MinimaLogger.log("Checking BlockChain.. descending");
			if(MinimaDB.getDB().getTxPoWTree() != null) {
				TxPoWTreeNode top = MinimaDB.getDB().getTxPoWTree().getTip();
				while(top != null) {
					TxBlock block = top.getTxBlock();
					
					//Get details
					TxPoW txp 			= block.getTxPoW();
					long blocknumber 	= txp.getBlockNumber().getAsLong();
					
					//Date string
					String date = MinimaLogger.DATEFORMAT.format(new Date(txp.getTimeMilli().getAsLong()));
					
					//Created
					ArrayList<Coin> outputs 		= block.getOutputCoins();
					for(Coin cc : outputs) {
						if(cc.getAddress().to0xString().equals(address)) {
							
							boolean found = true;
							if(!statecheck.equals("")) {
								//Check for state aswell..
								found = cc.checkForStateVariable(statecheck);
							}
							
							if(found) {
								MinimaLogger.log("BLOCK "+blocknumber+" CREATED COIN : "+cc.toString());
								
								JSONObject created = new JSONObject();
								created.put("block", blocknumber);
								created.put("date", date);
								created.put("datemilli", txp.getTimeMilli().toString());
								created.put("coin", cc.toJSON());
								outarr.add(created);
							}
						}
					}
					
					//Spent
					ArrayList<CoinProof> inputs  	= block.getInputCoinProofs();
					for(CoinProof incoin : inputs) {
						if(incoin.getCoin().getAddress().to0xString().equals(address)) {
							MinimaLogger.log("BLOCK "+blocknumber+" SPENT COIN : "+incoin.getCoin().toString());
							
							JSONObject spent = new JSONObject();
							spent.put("block", blocknumber);
							spent.put("date", date);
							spent.put("datemilli", txp.getTimeMilli().toString());
							spent.put("coin", incoin.getCoin().toJSON());
							inarr.add(spent);
						}
					}
					
					top = top.getParent();
				}
			}
			
			
			MinimaLogger.log("All checks complete..");
			
			resp.put("created", outarr);
			resp.put("spent", inarr);
			ret.put("coins", resp);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}
	
	public static void NotifyListener(MessageListener zListener, String zMessage) throws Exception {
		//Notify
		if(zListener != null) {
			
			//Details..
			JSONObject data = new JSONObject();
			data.put("message", zMessage);
			
			//Create the JSON Message
			JSONObject notify = new JSONObject();
			notify.put("event", "ARCHIVEUPDATE");
			notify.put("data", data);
			
			Message msg = new Message(NotifyManager.NOTIFY_POST);
			msg.addObject("notify", notify);
			
			//Notify them that something is happening..
			zListener.processMessage(msg);
		}
	}
	
	@Override
	public Command getFunction() {
		return new archive();
	}

	/**
	 * A special PING message to  check a valid connection..
	 */
	public static IBD sendArchiveReq(String zHost, int zPort, MiniNumber zStartBlock) {
		
		IBD ibd= null;
		
		int attempts = 0;
		
		while(attempts<3) {
			try {
				
				//Create the Network Message
				MiniData msg = NIOManager.createNIOMessage(NIOMessage.MSG_ARCHIVE_REQ, zStartBlock);
				
				//Open the socket..
				Socket sock = new Socket();
	
				//3 seconds to connect
				sock.connect(new InetSocketAddress(zHost, zPort), 10000);
				
				//10 seconds to read
				sock.setSoTimeout(10000);
				
				//Create the streams..
				OutputStream out 		= sock.getOutputStream();
				DataOutputStream dos 	= new DataOutputStream(out);
				
				InputStream in			= sock.getInputStream();
				DataInputStream dis 	= new DataInputStream(in);
				
				//Write the data
				msg.writeDataStream(dos);
				dos.flush();
				
				//Tell the NIO
				Main.getInstance().getNIOManager().getTrafficListener().addWriteBytes("sendArchiveReq",msg.getLength());
				
				//Load the message
				MiniData resp = MiniData.ReadFromStream(dis);
				
				//Tell the NIO
				Main.getInstance().getNIOManager().getTrafficListener().addReadBytes("sendArchiveReq",resp.getLength());
				
				//Close the streams..
				dis.close();
				in.close();
				dos.close();
				out.close();
				
				//Convert
				ByteArrayInputStream bais 	= new ByteArrayInputStream(resp.getBytes());
				DataInputStream bdis 		= new DataInputStream(bais);
	
				//What Type..
				MiniByte type = MiniByte.ReadFromStream(bdis);
				
				//Load the IBD
				ibd = IBD.ReadFromStream(bdis);
				
				bdis.close();
				bais.close();
			
				break;
				
			}catch(Exception exc){
				MinimaLogger.log("Archive connection : "+exc+" @ "+zHost+":"+zPort);
				
				//Null the IBD
				ibd= null;
				
				//Increase attempts
				attempts++;			
				
				if(attempts<3) {
					MinimaLogger.log(attempts+" Attempts > Wait 10 seconds and re-attempt..");
					
					//Wait 10 seconds
					try {Thread.sleep(10000);} catch (InterruptedException e) {}
					
					MinimaLogger.log("Re-attempt started..");
				}
			}
		}
		
		return ibd;
	}
}
