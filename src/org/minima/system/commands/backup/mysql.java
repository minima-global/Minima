package org.minima.system.commands.backup;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.UserDB;
import org.minima.database.wallet.Wallet;
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
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.mysql.MySQLConnect;

public class mysql extends Command {

	public mysql() {
		super("mysql","Store and resync the archive data in a MySQL database");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmysql\n"
				+ "\n"
				+ "Export the archive data of this node to a MySQL database.\n"
				+ "\n"
				+ "The MySQL db can be used to perform a chain re-sync to put users on the correct chain,\n"
				+ "\n"
				+ "or a seed re-sync to restore access to lost funds, using the 24 word seed phrase.\n"
				+ "\n"
				+ "Can also be used to query an address for its history of spent and unspent coins.\n"
				+ "\n"
				+ "host:\n"
				+ "    The ip:port (or name of Docker container) running the MySQL db.\n"
				+ "\n"
				+ "database:\n"
				+ "    name of the MySQL db being used to store the archive db data.\n"
				+ "\n"
				+ "user:\n"
				+ "    MySQL user to login to as.\n"
				+ "\n"
				+ "password:\n"
				+ "    MySQL password for the user provided.\n"
				+ "\n"
				+ "action:\n"
				+ "    info : Show the blocks stored in the archive db and compare to the MySQL db.\n"
				+ "    integrity : Check the block order and block parents are correct in the MySQL db.\n"
				+ "    update : Update the MySQL db with the latest syncblocks from the node's archive db.\n"
				+ "    addresscheck : Check the history of all the spent and unspent coins from an address.\n"
				+ "    autobackup : Automatically save archive data to MySQL DB. Use with enable.\n"
				+ "    resync : Perform a chain or seed re-sync from the specified MySQL db.\n"
				+ "             Will shutdown the node so you must restart it once complete.\n"
				+ "    wipe :  Be careful. Wipe the MySQL db.\n"
				+ "\n"
				+ "phrase: (optional)\n"
				+ "     Use with action:resync. The BIP39 seed phrase of the node to re-sync.\n"
				+ "     If provided, the node will be wiped and re-synced.\n"
				+ "     If not provided, the node will be re-synced to the chain and will not be wiped.\n"
				+ "\n"
				+ "keys: (optional)\n"
				+ "    If the seed phrase is provided, optionally set the number of keys to create.\n"
				+ "    Default is 80.\n"
				+ "\n"
				+ "keyuses: (optional)\n"
				+ "    If the seed phrase is provided, optionally set the number of previous uses for each key created.\n"
				+ "    Default is 1000.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    Use with action:checkaddress. The address to check the history of spent and unspent coins for.\n"
				+ "\n"
				+ "enable: (optional)\n"
				+ "    Use with action:autobackup. Automatically save data to MySQL archive DB.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:info\n"
				+ "\n"
				+ "mysql host:dockermysql database:archivedb user:archiveuser password:archivepassword action:info\n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:integrity\n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:update\n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:addresscheck address:MxG08.. \n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:resync\n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:resync phrase:\"24 WORDS HERE\" keys:90 keyuses:2000\n"
				+ "\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","database","user","password","keys","keyuses","phrase","address","enable"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Are we on mobile..
		if(GeneralParams.IS_MOBILE) {
			throw new CommandException("Sorry - MySQL does not work on Android..");
		}
		
		//What action are we doing..
		String action = getParam("action","info");
		
		//Get the details
		String host 		= getParam("host");
		String db 			= getParam("database");
		String user 		= getParam("user");
		String password 	= getParam("password");
		
		//Get the login details..
		MySQLConnect mysql = new MySQLConnect(host, db, user, password);
		mysql.init();
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		if(action.equals("info")) {
			
			//Check the DB
			long firstblock = mysql.loadFirstBlock();
			long lastblock 	= mysql.loadLastBlock();
			long total 		= firstblock-lastblock; 
			
			TxBlock archlastblock 	= arch.loadLastBlock();
			TxBlock archfirstblock	= arch.loadFirstBlock();
			long archtotal 			= 0;
			JSONObject archresp = new JSONObject();
			archresp.put("archivestart", -1);
			archresp.put("archiveend", -1);
			archresp.put("archivetotal", 0);
			
			if(archlastblock != null || archfirstblock!=null) {
				archtotal = archfirstblock.getTxPoW().getBlockNumber().sub(archlastblock.getTxPoW().getBlockNumber()).getAsLong();
				archresp.put("archivestart", archlastblock.getTxPoW().getBlockNumber().getAsLong());
				archresp.put("archiveend", archfirstblock.getTxPoW().getBlockNumber().getAsLong());
				archresp.put("archivetotal", archtotal);
			}
			
			JSONObject mysqlresp = new JSONObject();
			mysqlresp.put("mysqlstart", lastblock);
			mysqlresp.put("mysqlend", firstblock);
			mysqlresp.put("mysqltotal", total);
			
			JSONObject resp = new JSONObject();
			resp.put("archive", archresp);
			resp.put("mysql", mysqlresp);
			
			boolean autobackup = MinimaDB.getDB().getUserDB().getAutoBackupMySQL();
			resp.put("autobackup", autobackup);
			
			ret.put("response", resp);
		
		}else if(action.equals("wipe")) {
			
			//First wipe the DB
			mysql.wipeAll();
			
			ret.put("response", "MySQL DB Wiped..");
		
		}else if(action.equals("autobackup")) {
			
			UserDB udb = MinimaDB.getDB().getUserDB();
			
			boolean enable = getBooleanParam("enable");
			udb.setAutoBackupMySQL(enable);
			
			MinimaLogger.log("ENABLE:"+enable);
			
			if(enable) {
				udb.setAutoMySQLHost(host);
				udb.setAutoMySQLDB(db);
				udb.setAutoMySQLUser(user);
				udb.setAutoMySQLPassword(password);
			}else {
				udb.setAutoMySQLHost("");
				udb.setAutoMySQLDB("");
				udb.setAutoMySQLUser("");
				udb.setAutoMySQLPassword("");
			}
			
			JSONObject resp = new JSONObject();
			boolean autobackup = MinimaDB.getDB().getUserDB().getAutoBackupMySQL();
			resp.put("autobackup", autobackup);
			
			ret.put("response", resp);
			
		}else if(action.equals("integrity")) {
			
			//Check all the block in the MySQL DB..
			long mysqllastblock 	= mysql.loadLastBlock();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			//Load a range..
			long firstblock = -1;
			long endblock 	= -1;
			TxBlock lastblock = null;
			
			long startload 	= mysqllastblock; 
			while(true) {
				MinimaLogger.log("MySQL Verifying from : "+startload);
				ArrayList<TxBlock> blocks = mysql.loadBlockRange(new MiniNumber(startload));
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				for(TxBlock block : blocks) {
					
					if(lastblock == null) {
						firstblock = block.getTxPoW().getBlockNumber().getAsLong();
					}else {
						if(!block.getTxPoW().getParentID().isEqual(lastblock.getTxPoW().getTxPoWIDData())) {
							throw new CommandException("ERROR : block parents are incorrect @ "+block.getTxPoW().getBlockNumber().toString());
						}
						
						if(!block.getTxPoW().getBlockNumber().isEqual(lastblock.getTxPoW().getBlockNumber().increment())) {
							throw new CommandException("ERROR : block numbers are incorrect @ "+block.getTxPoW().getBlockNumber().toString());
						}
						
					}
					lastblock = block;
					
					endblock = block.getTxPoW().getBlockNumber().getAsLong();
				}
				
				startload = endblock+1; 
			}
			
			JSONObject resp = new JSONObject();
			resp.put("start", firstblock);
			resp.put("end", endblock);
			
			ret.put("response", resp);
			
		}else if(action.equals("update")) {
			
			//Do we have any block
			int size = arch.getSize();
			if(size==0) {
				throw new CommandException("No blocks in ArchiveDB");
			}
			
			//Load the files from Archive..
			long firstarch			= arch.loadFirstBlock().getTxPoW().getBlockNumber().getAsLong();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			if(mysqlfirstblock>firstarch-1) {
				throw new CommandException("Archive nodes to low.. cannot sync arch:"+firstarch+" mysql:"+mysqlfirstblock);
			}
			
			//How many to copy..
			long startload 	= mysqlfirstblock;
			if(mysqlfirstblock == -1) {
				startload = 0;
			}
			
			boolean finished = false;
			TxBlock lastblock = null;
			while(!finished){
			
				long ender = startload+100;
				MinimaLogger.log("Transfer from "+startload+" to "+(ender-1));
				
				//Load blocks from the archive
				ArrayList<TxBlock> blocks = arch.loadBlockRange(new MiniNumber(startload), new MiniNumber(ender),false);
				if(blocks.size()==0) {
					//All blocks injected.. 
					MinimaLogger.log("All blocks added");
					finished = true;
					break;
				}
				
				//Now write these to the DB
				for(TxBlock block : blocks) {
					
					//Check valid..
					if(lastblock!=null) {
						
						//Make sure the parent is correct
						if(!block.getTxPoW().getParentID().isEqual(lastblock.getTxPoW().getTxPoWIDData())) {
							throw new CommandException("ERROR : block parents are incorrect @ "+block.getTxPoW().getBlockNumber().toString());
						}
						
						if(!block.getTxPoW().getBlockNumber().isEqual(lastblock.getTxPoW().getBlockNumber().increment())) {
							throw new CommandException("ERROR : block numbers are incorrect @ "+block.getTxPoW().getBlockNumber().toString());
						}
					}
					lastblock = block;
					
					long saveblock = block.getTxPoW().getBlockNumber().getAsLong();
					
					//Save to MySQL..
					mysql.saveBlock(block);
					
					//What block is this
					if(saveblock>startload) {
						startload = saveblock; 
					}
				}
			}
			
			//Check the DB
			long firstblock 	= mysql.loadFirstBlock();
			long mylastblock 	= mysql.loadLastBlock();
			long total 			= firstblock-mylastblock; 
			
			TxBlock archlastblock 	= arch.loadLastBlock();
			TxBlock archfirstblock	= arch.loadFirstBlock();
			long archtotal 			= archfirstblock.getTxPoW().getBlockNumber().sub(archlastblock.getTxPoW().getBlockNumber()).getAsLong();
			
			JSONObject mysqlresp = new JSONObject();
			mysqlresp.put("mysqlstart", mylastblock);
			mysqlresp.put("mysqlend", firstblock);
			mysqlresp.put("mysqltotal", total);
			
			JSONObject archresp = new JSONObject();
			archresp.put("archivestart", archlastblock.getTxPoW().getBlockNumber().getAsLong());
			archresp.put("archiveend", archfirstblock.getTxPoW().getBlockNumber().getAsLong());
			archresp.put("archivetotal", archtotal);
			
			JSONObject resp = new JSONObject();
			resp.put("archive", archresp);
			resp.put("mysql", mysqlresp);
			
			ret.put("response", resp);
			
		}else if(action.equals("resync")) {
			
			//Can only do this if all keys created..
			vault.checkAllKeysCreated();
			
			//How many Keys do we need to generate
			int keys = getNumberParam("keys", new MiniNumber(Wallet.NUMBER_GETADDRESS_KEYS + 16)).getAsInt();
			
			//Set the key uses to this..
			int keyuses = getNumberParam("keyuses", new MiniNumber(1000)).getAsInt();
			
			//Are we resetting the wallet too ?
			MiniData seed 		= null;
			String phrase = getParam("phrase","");
			
			//Clean it up..
			String cleanphrase = BIP39.cleanSeedPhrase(phrase);
			
			if(!phrase.equals("")) {
				
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
//					NotifyListener(minimalistener,"Creating key "+i);
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
			MiniNumber endblock 	= MiniNumber.ZERO;
			boolean foundsome 		= false;
			boolean firstrun 		= true;
			MiniNumber firstStart   = MiniNumber.ZERO;
			
			int counter = 0;
			MinimaLogger.log("System clean..");
			System.gc();
			while(true) {
				
				//We don't need any transactions in RamDB
				MinimaDB.getDB().getTxPoWDB().wipeDBRAM();
				
				//Clean system counter
				counter++;
				if(counter % 20 == 0) {
					Main.getInstance().resetMemFull();
				}
				
				//Create an IBD for the mysql data
				ArrayList<TxBlock> mysqlblocks = mysql.loadBlockRange(startblock);
				
				IBD ibd = new IBD();
				ibd.setTxBlocks(mysqlblocks);
				
//				//Is there a cascade..
//				if(startblock.isEqual(MiniNumber.ZERO) && ibd.hasCascade()) {
//					MinimaLogger.log("Cascade Received.. "+ibd.getCascade().getTip().getTxPoW().getBlockNumber());
//					
//					//Set it as our cascade
//					MinimaDB.getDB().setIBDCascade(ibd.getCascade());
//					
//					//Do we need to save this..
//					MinimaDB.getDB().getArchive().checkCascadeRequired(ibd.getCascade());
//				}
				
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
					
					MinimaLogger.log("Archive IBD received start : "+start.getTxPoW().getBlockNumber()+" end : "+endblock);
				
//					//Notify the Android Listener
//					NotifyListener(minimalistener,"Loading "+start.getTxPoW().getBlockNumber()+" @ "+new Date(start.getTxPoW().getTimeMilli().getAsLong()).toString());
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
					if(attempts>128) {
						error = true;
						break;
					}
				}
				
				if(error) {
					MinimaLogger.log("ERROR : There was an error processing that IBD");
					break;
				}
				
				//Now wait to catch up..
				MinimaLogger.log("Waiting for chain to catch up.. please wait");
				attempts = 0;
				while(foundsome) {
					if(!tip.getBlockNumber().isEqual(endblock)) {
						Thread.sleep(250);
					}else {
						break;
					}
					
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
					
					attempts++;
					if(attempts>1024) {
						error = true;
						break;
					}
				}
				
				if(error) {
					MinimaLogger.log("ERROR : There was an error processing that IBD");
					break;
				}
				
				//Do we have enough to ask again.. 
				if(size==0) {
					break;
				}
			}
			
			//Notify the Android Listener
//			NotifyListener(minimalistener,"All blocks loaded.. pls wait");
			MinimaLogger.log("All Archive data received and processed.. shutting down.."); 
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive sync completed.. shutting down now.. please restart after");
			resp.put("start", firstStart.toString());
			resp.put("end", endblock.toString());
			
			if(!phrase.equals("")) {
				resp.put("phrase", phrase);
				resp.put("clean", cleanphrase);
			}
			
			ret.put("response", resp);
			
			//And NOW shut down..
			Main.getInstance().getTxPoWProcessor().stopMessageProcessor();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
		}else if(action.equals("addresscheck")) {
			
			//Which address are we looking for
			String address = getAddressParam("address");
			
			//Cycle through
			JSONObject resp 	= new JSONObject();
			JSONArray inarr 	= new JSONArray();
			JSONArray outarr 	= new JSONArray();
			MiniNumber firstStart   = MiniNumber.ZERO;
			while(true) {
				
				//Create an IBD for the mysql data
				ArrayList<TxBlock> mysqlblocks = mysql.loadBlockRange(firstStart);
				if(mysqlblocks.size()==0) {
					//No blocks left
					break;
				}
				
				for(TxBlock block : mysqlblocks) {
					
					TxPoW txp 			= block.getTxPoW();
					long blocknumber 	= txp.getBlockNumber().getAsLong();
					
					//Created
					ArrayList<Coin> outputs 		= block.getOutputCoins();
					for(Coin cc : outputs) {
						if(cc.getAddress().to0xString().equals(address)) {
							MinimaLogger.log("BLOCK "+blocknumber+" CREATED COIN : "+cc.toString());
							
							JSONObject created = new JSONObject();
							created.put("block", blocknumber);
							created.put("coin", cc.toJSON());
							outarr.add(created);
						}
					}
					
					//Spent
					ArrayList<CoinProof> inputs  	= block.getInputCoinProofs();
					for(CoinProof incoin : inputs) {
						if(incoin.getCoin().getAddress().to0xString().equals(address)) {
							MinimaLogger.log("BLOCK "+blocknumber+" SPENT COIN : "+incoin.getCoin().toString());
							
							JSONObject spent = new JSONObject();
							spent.put("block", blocknumber);
							spent.put("coin", incoin.getCoin().toJSON());
							inarr.add(spent);
						}
					}
					
					//Start from here
					firstStart = txp.getBlockNumber().increment();
				}
			}
			
			resp.put("created", outarr);
			resp.put("spent", inarr);
			ret.put("coins", resp);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		//Then shutdown
		mysql.shutdown();
		
		return ret;
	}
	
	
	@Override
	public Command getFunction() {
		return new mysql();
	}	
}
