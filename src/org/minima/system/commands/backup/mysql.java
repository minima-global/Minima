package org.minima.system.commands.backup;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;
import java.util.zip.GZIPOutputStream;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.archive.RawArchiveInput;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.UserDB;
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
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
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
				+ "Export archive data to a MySQL server.\n"
				+ "\n"
				+ "The MySQL db can be used to perform a chain re-sync to put users on the correct chain,\n"
				+ "\n"
				+ "or a seed re-sync to restore access to lost funds, using the seed phrase.\n"
				+ "\n"
				+ "Can query an address for its history of spent and unspent coins.\n"
				+ "\n"
				+ "Additionally export the MySQL db to a gzip file for resyncing with 'reset' or 'archive' command.\n"
				+ "\n"
				+ "You can use setlogin to auto set the login details"
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
				+ "readonly:\n"
				+ "    true or false, Connect in readonly mode.\n"
				+ "logs:\n"
				+ "    Show detailed logs - default true.\n"
				+ "\n"
				+ "action:\n"
				+ "    info : Show the blocks stored in the archive db and compare to the MySQL db.\n"
				+ "    integrity : Check the block order and block parents are correct in the MySQL db.\n"
				+ "    update : Update the MySQL db with the latest syncblocks from the node's archive db.\n"
				+ "    addresscheck : Check the history of all the spent and unspent coins from an address.\n"
				+ "    setlogin : Set the MySQL login details so you don't need to type them in every time.\n"
				+ "    clearlogin : Clear MySQL login details.\n"
				+ "    autobackup : Automatically save archive data to MySQL DB. Use with enable. Also stores all TxPoW the node sees.\n"
				+ "    findtxpow : Search for an individual TxPoW (only works if autobackup is enabled).\n"
				+ "    resync : Perform a chain or seed re-sync from the specified MySQL db.\n"
				+ "             Will shutdown the node so you must restart it once complete.\n"
				+ "    wipe :  Be careful. Wipe the MySQL db.\n"
				+ "    h2export : export the MySQL db to an archive gzip file which can be used to resync a node.\n"
				+ "    h2import : import an archive gzip file to the MySQL db.\n"
				+ "    rawexport : export the MySQL db to raw.dat file which can be used to resync a node (faster than H2).\n"
				+ "    rawimport : import a raw.dat to the MySQL db.\n"
				+ "\n"
				+ "phrase: (optional)\n"
				+ "     Use with action:resync. The 24 word seed phrase of the node to re-sync.\n"
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
				+ "    Use with action:addresscheck. The address to check the history of spent and unspent coins for.\n"
				+ "\n"
				+ "enable: (optional)\n"
				+ "    Use with action:autobackup. Automatically save data to MySQL archive DB.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    Name or path of the archive gzip file to export to or import from.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "mysql ..LOGIN_DETAILS.. action:setlogin\n"
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:info\n"
				+ "\n"
				+ "mysql host:dockermysql database:archivedb user:archiveuser password:archivepassword action:info\n"
				+ "\n"
				+ "mysql (If you have not setlogin)..LOGIN_DETAILS.. action:integrity\n"
				+ "\n"
				+ "mysql action:update\n"
				+ "\n"
				+ "mysql action:addresscheck address:MxG08.. \n"
				+ "\n"
				+ "mysql action:resync\n"
				+ "\n"
				+ "mysql action:findtxpow txpowid:0x00FFEEDD..\n"
				+ "\n"
				+ "mysql action:resync phrase:\"24 WORDS HERE\" keys:90 keyuses:2000\n"
				+ "\n"
				+ "mysql action:h2export file:archivexport-DDMMYY.gzip\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","database",
				"user","password","keys","keyuses","phrase","address","txpowid",
				"enable","file","statecheck","logs","maxexport",
				"readonly","startfix","endfix","block"}));
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
		UserDB udb 			= MinimaDB.getDB().getUserDB();
		
		String host 		= "";
		String db 			= "";
		String user 		= "";
		String password 	= "";
		
		boolean autologindetail = MinimaDB.getDB().getUserDB().getAutoLoginDetailsMySQL();
		if(autologindetail) {
			host 		= udb.getAutoMySQLHost();
			db 			= udb.getAutoMySQLDB();
			user 		= udb.getAutoMySQLUser();
			password 	= udb.getAutoMySQLPassword();
		}else {
			host 		= getParam("host");
			db 			= getParam("database");
			user 		= getParam("user");
			password 	= getParam("password");
		}
		
		//Are logs enabled
		boolean logs		= getBooleanParam("logs", true);
		
		//Get the login details..
		boolean readonly 	= getBooleanParam("readonly", false);
		MySQLConnect mysql 	= new MySQLConnect(host, db, user, password, readonly);
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
			
			boolean logindetails = MinimaDB.getDB().getUserDB().getAutoLoginDetailsMySQL();
			resp.put("logindetails", logindetails);
			resp.put("user", user);
			resp.put("password", "***");
			resp.put("host", host);
			resp.put("database", db);
			
			ret.put("response", resp);
		
		}else if(action.equals("wipe")) {
			
			//First wipe the DB
			mysql.wipeAll();
			
			ret.put("response", "MySQL DB Wiped..");
		
		}else if(action.equals("setlogin")) {
			
			udb.setAutoMySQLHost(host);
			udb.setAutoMySQLDB(db);
			udb.setAutoMySQLUser(user);
			udb.setAutoMySQLPassword(password);
			
			udb.setAutoLoginDetailsMySQL(true);
			
			JSONObject resp = new JSONObject();
			boolean autologin = udb.getAutoLoginDetailsMySQL();
			resp.put("logindetails", autologin);
			resp.put("user", user);
			resp.put("password", "***");
			resp.put("host", host);
			resp.put("database", db);
			
			ret.put("response", resp);
			
		}else if(action.equals("clearlogin")) {
			
			udb.setAutoMySQLHost("");
			udb.setAutoMySQLDB("");
			udb.setAutoMySQLUser("");
			udb.setAutoMySQLPassword("");
			
			udb.setAutoLoginDetailsMySQL(false);
			
			JSONObject resp = new JSONObject();
			boolean autologin = udb.getAutoLoginDetailsMySQL();
			resp.put("logindetails", autologin);
			
			ret.put("response", resp);
			
		}else if(action.equals("autobackup")) {
			
			boolean enable = getBooleanParam("enable");
			udb.setAutoBackupMySQL(enable);
			
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
				if(logs) {
					MinimaLogger.log("MySQL Verifying from : "+startload, false);
				}
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
			long lastarch			= arch.loadLastBlock().getTxPoW().getBlockNumber().getAsLong();
			long firstarch			= arch.loadFirstBlock().getTxPoW().getBlockNumber().getAsLong();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			if(mysqlfirstblock>firstarch-1) {
				throw new CommandException("Archive nodes to low.. cannot sync arch:"+firstarch+" mysql:"+mysqlfirstblock);
			}
			
			//How many to copy..
			long startload 	= mysqlfirstblock;
			if(mysqlfirstblock == -1) {
				startload = 0;
				
				//Does the archive start at 0..
				if(lastarch != 0 || lastarch != 1) {
					MinimaLogger.log("Archive NOT starting from 0!.. Load from start of archive @ "+lastarch);
					startload = lastarch; 
				}
			}
			
			boolean finished = false;
			TxBlock lastblock = null;
			while(!finished){
			
				long ender = startload+100;
				if(logs) {
					MinimaLogger.log("Transfer from "+startload+" to "+(ender-1));
				}
				
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
			
			//Tell the MiniDAPPs..
			Main.getInstance().PostNotifyEvent("MDS_RESYNC_START",new JSONObject());
			
			//How many Keys do we need to generate
			int keys = getNumberParam("keys", new MiniNumber(Wallet.NUMBER_GETADDRESS_KEYS)).getAsInt();
			
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
						Thread.sleep(50);
					}else {
						break;
					}
					
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
					
					attempts++;
					if(attempts>10000) {
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
				
//				//HACK
//				if(startblock.isMore(new MiniNumber(5000))) {
//					break;
//				}
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
			//Main.getInstance().getTxPoWProcessor().stopMessageProcessor();
			Main.getInstance().shutdownFinalProcs();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
			//Tell listener..
			Main.getInstance().NotifyMainListenerOfShutDown();
			
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
					String txpid 		= block.getTxPoW().getTxPoWID();
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
								created.put("blockid", txpid);
								created.put("date", date);
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
								spent.put("blockid", txpid);
								spent.put("date", date);
								spent.put("coin", incoin.getCoin().toJSON());
								inarr.add(spent);
							}
						}
					}
					
					//Start from here
					firstStart = txp.getBlockNumber().increment();
				}
			}
			
			resp.put("created", outarr);
			resp.put("spent", inarr);
			ret.put("coins", resp);
		
		}else if(action.equals("h2import")) {
			
			long timestart = System.currentTimeMillis();
			
			//Create a temp name
			String infile 	= getParam("file");
			File fileinfile = MiniFile.createBaseFile(infile);
			
			//Create  tyemp DB
			ArchiveManager archtemp = new ArchiveManager();
			
			//Create a temp DB file..
			File restorefolder = new File(GeneralParams.DATA_FOLDER,"archiverestore");
			restorefolder.mkdirs();
			
			File tempdb = new File(restorefolder,"archivetemp");
			if(tempdb.exists()) {
				tempdb.delete();
			}
			archtemp.loadDB(tempdb);
			
			//And now restore..
			MinimaLogger.log("Restoring H2 Archive DB..");
			archtemp.restoreFromFile(fileinfile, true);
			
			//Wipe the old data
			mysql.wipeAll();
			
			//Is there a cascade..
			Cascade casc = archtemp.loadCascade();
			if(casc != null) {
				MinimaLogger.log("Cascade found.. ");
				mysql.saveCascade(casc);
			}
			
			//Load the H2 Data
			long mysqllastblock 	= archtemp.loadLastBlock().getTxPoW().getBlockNumber().getAsLong();
			long mysqlfirstblock 	= archtemp.loadFirstBlock().getTxPoW().getBlockNumber().getAsLong();
			
			MinimaLogger.log("First block:"+mysqllastblock);
			MinimaLogger.log("Last block:"+mysqlfirstblock);
			
			//Load a range..
			long endblock 	= -1;
			TxBlock lastblock = null;
			
			long startload 	= mysqllastblock-1;
			int counter = 0;
			while(true) {
				if(logs) {
					MinimaLogger.log("Loading from H2 @ "+startload);
				}
				long endload = startload+250;
				
				ArrayList<TxBlock> blocks = archtemp.loadBlockRange(new MiniNumber(startload), new MiniNumber(endload),false);
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					//Send to H2
					mysql.saveBlock(block);
					//MinimaLogger.log("Save block : "+block.getTxPoW().getBlockNumber());
					
					//New firstblock
					startload 	= block.getTxPoW().getBlockNumber().getAsLong();
				}
				endblock = startload; 
				
				//Clean up..
				counter++;
				if(counter % 10 == 0) {
					System.gc();
				}
			}
			
			//Shutdown TEMP DB
			archtemp.saveDB(false);
			
			//Delete the restore folder
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
			
			long timediff = System.currentTimeMillis() - timestart;
			
			JSONObject resp = new JSONObject();
			resp.put("start", mysqllastblock-1);
			resp.put("end", endblock);
			resp.put("time", MiniFormat.ConvertMilliToTime(timediff));
			
			ret.put("response", resp);
			
		}else if(action.equals("h2export")) {
			
			//Create a temp name
			String outfile = getParam("file","archivebackup-"+System.currentTimeMillis()+".h2.gzip");
			
			//Create the file
			File gzoutput = MiniFile.createBaseFile(outfile);
			if(gzoutput.exists()) {
				gzoutput.delete();
			}
			
			//Create  tyemp DB
			ArchiveManager archtemp = new ArchiveManager();
			
			//Create a temp DB file..
			File restorefolder = new File(GeneralParams.DATA_FOLDER,"archiverestore");
			restorefolder.mkdirs();
			
			File tempdb = new File(restorefolder,"archivetemp");
			if(tempdb.exists()) {
				tempdb.delete();
			}
			archtemp.loadDB(tempdb);
			
			//Load the MySQL and output to the H2
			long mysqllastblock 	= mysql.loadLastBlock();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			//Load a range..
			long firstblock = -1;
			long endblock 	= -1;
			TxBlock lastblock = null;
			
			long startload 	= mysqllastblock;
			int counter = 0;
			while(true) {
				if(logs) {
					MinimaLogger.log("Loading from MySQL @ "+startload);
				}
				ArrayList<TxBlock> blocks = mysql.loadBlockRange(new MiniNumber(startload));
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					//Send to H2
					archtemp.saveBlock(block);
					
					if(lastblock == null) {
						firstblock = block.getTxPoW().getBlockNumber().getAsLong();
					}
					lastblock = block;
					endblock = block.getTxPoW().getBlockNumber().getAsLong();
				}
				
				startload = endblock+1;
				
				//Clean up..
				counter++;
				if(counter % 20 == 0) {
					System.gc();
				}
			}

			//Load the cascade if it is there
			Cascade casc = mysql.loadCascade();
			if(casc!=null) {
				archtemp.checkCascadeRequired(casc);
			}else {
				MinimaLogger.log("No cascade found in MySQL..");
			}
			
			//And Now export to File..
			MinimaLogger.log("Exporting to H2 SQL file..");
			archtemp.backupToFile(gzoutput,true);
			
			//Shutdown TEMP DB
			archtemp.saveDB(false);
			
			//Delete the restore folder
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
			
			JSONObject resp = new JSONObject();
			resp.put("start", firstblock);
			resp.put("end", endblock);
			resp.put("file", gzoutput.getName());
			resp.put("path", gzoutput.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(gzoutput.length()));
			
			ret.put("response", resp);
		
		}else if(action.equals("size")) {
			
			int total = mysql.getCount();
			
			JSONObject resp = new JSONObject();
			resp.put("size", total);
			ret.put("response", resp);
			
		}else if(action.equals("rawexport")) {
			
			//Create a temp name
			String outfile = getParam("file","archivebackup-"+System.currentTimeMillis()+".raw.dat");
			
			//Create the file
			File rawoutput = MiniFile.createBaseFile(outfile);
			if(rawoutput.exists()) {
				rawoutput.delete();
			}
			
			//Create output streams..
			FileOutputStream fix 		= new FileOutputStream(rawoutput);
			BufferedOutputStream bos 	= new BufferedOutputStream(fix, 65536);
			GZIPOutputStream gout 		= new GZIPOutputStream(bos, 65536);
			DataOutputStream dos 		= new DataOutputStream(gout);
			
			//Load the cascade if it is there
			Cascade casc = mysql.loadCascade();
			if(casc!=null) {
				MinimaLogger.log("Cascade found in MySQL..");
				
				//Write cacade out..
				MiniByte.TRUE.writeDataStream(dos);
				casc.writeDataStream(dos);
			}else {
				MiniByte.FALSE.writeDataStream(dos);
				MinimaLogger.log("No cascade found in MySQL..");
			}
			
			//How many entries..
			int total = mysql.getCount();
			MinimaLogger.log("Total records found : "+total);
			
			//Max specified..
			if(existsParam("maxexport")) {
				int max = getNumberParam("maxexport").getAsInt();
				MinimaLogger.log("Max export specified.. : "+max);
				if(total>max) {
					total = max;
				}
			}
			
			MiniNumber tot = new MiniNumber(total);
			tot.writeDataStream(dos);
			
			int outcounter = 0;
			
			//Load the MySQL and output to the H2
			long mysqllastblock 	= mysql.loadLastBlock();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			//Load a range..
			long firstblock = -1;
			long endblock 	= -1;
			TxBlock lastblock = null;
			
			long startload 	= mysqllastblock;
			int counter = 0;
			while(true) {
				
				//Small log message
				if(counter % 20 == 0) {
					if(logs) {
						MinimaLogger.log("Loading from MySQL @ "+startload);
					}
				}
				
				ArrayList<TxBlock> blocks = mysql.loadBlockRange(new MiniNumber(startload));
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					//Send to data export file..
					block.writeDataStream(dos);
					//archtemp.saveBlock(block);
					
					if(lastblock == null) {
						firstblock = block.getTxPoW().getBlockNumber().getAsLong();
					}
					lastblock = block;
					endblock = block.getTxPoW().getBlockNumber().getAsLong();
					
					//Increase counter
					outcounter++;
					if(outcounter>=total) {
						break;
					}
				}
				
				if(outcounter>=total) {
					MinimaLogger.log("Finished loading blocks..");
					break;
				}
				
				startload = endblock+1;
				
				//Clean up..
				counter++;
				if(counter % 20 == 0) {
					System.gc();
				}
			}			
			
			//Flush data
			dos.flush();
			
			try {
				dos.close();
				gout.close();
				bos.close();
				fix.close();
			}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
			
			JSONObject resp = new JSONObject();
			resp.put("start", firstblock);
			resp.put("end", endblock);
			resp.put("file", rawoutput.getName());
			resp.put("path", rawoutput.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(rawoutput.length()));
			
			ret.put("response", resp);
				
		}else if(action.equals("rawimport")) {
			
			long timestart = System.currentTimeMillis();
			
			//Create a temp name
			String infile 	= getParam("file");
			File fileinfile = MiniFile.createBaseFile(infile);
			
			RawArchiveInput rawin = new RawArchiveInput(fileinfile);
			rawin.connect();
			
			//Wipe the old data
			mysql.wipeAll();
			
			//Is there a cascade..
			Cascade casc = rawin.getCascade();
			if(casc != null) {
				MinimaLogger.log("Cascade found.. ");
				mysql.saveCascade(casc);
			}
			
			//Load a range..
			long endblock 	= -1;
			TxBlock lastblock = null;
			int counter = 0;
			while(true) {
				//Get the next batch of data..
				IBD syncibd 				= rawin.getNextIBD();
				ArrayList<TxBlock> blocks 	= syncibd.getTxBlocks();
				
				if(logs) {
					if(counter % 10 ==0) {
						if(blocks.size()>0) {
							MinimaLogger.log("Loading from RAW Block : "+blocks.get(0).getTxPoW().getBlockNumber());
						}
					}
				}
				
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					//Send to MySQL
					mysql.saveBlock(block);
					//MinimaLogger.log("Save block : "+block.getTxPoW().getBlockNumber());
				}
				
				//Clean up..
				counter++;
				if(counter % 10 == 0) {
					System.gc();
				}
			}
			
			//Shutdown TEMP DB
			rawin.stop();
			
			long timediff = System.currentTimeMillis() - timestart;
			
			JSONObject resp = new JSONObject();
			resp.put("time", MiniFormat.ConvertMilliToTime(timediff));
			
			ret.put("response", resp);
			
		}else if(action.equals("fixmissing")) {
			
			long timestart = System.currentTimeMillis();
			
			//Create a temp name
			String infile 	= getParam("file");
			File fileinfile = MiniFile.createBaseFile(infile);
			
			RawArchiveInput rawin = new RawArchiveInput(fileinfile);
			rawin.connect();
			
			//Start and end blocks
			long startchecking = 0;
			if(existsParam("startfix")) {
				startchecking = getNumberParam("startfix").getAsLong();
			}
			
			//Is there a cascade..
			/*Cascade casc = rawin.getCascade();
			if(casc != null) {
				MinimaLogger.log("Cascade found.. ");
				mysql.saveCascade(casc);
			}*/
			
			//Load a range..
			long endblock 	= -1;
			TxBlock lastblock = null;
			int counter = 0;
			
			boolean savingblocks = false;
			int totalsaved = 0; 
			while(true) {
				//Get the next batch of data..
				IBD syncibd 				= rawin.getNextIBD();
				ArrayList<TxBlock> blocks 	= syncibd.getTxBlocks();
				
				if(logs) {
					if(counter % 10 ==0) {
						if(blocks.size()>0) {
							MinimaLogger.log("Loading from RAW Block : "+blocks.get(0).getTxPoW().getBlockNumber()+" SAVING:"+savingblocks,false);
						}
					}
				}
				
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					//Which block is this..
					long blocknum = block.getTxPoW().getBlockNumber().getAsLong();
					
					if(blocknum > startchecking) {
						savingblocks = true;
						
						//Get the block..
						TxBlock txblk = mysql.loadBlockFromNum(blocknum);
						
						if(txblk == null) {
							mysql.saveBlock(block);
							totalsaved++;
							MinimaLogger.log("Save MISSING block : "+blocknum);
						}
					}
				}
				
				//Clean up..
				counter++;
				if(counter % 10 == 0) {
					System.gc();
				}
			}
			
			//Shutdown TEMP DB
			rawin.stop();
			
			long timediff = System.currentTimeMillis() - timestart;
			
			JSONObject resp = new JSONObject();
			resp.put("missing", totalsaved);
			resp.put("time", MiniFormat.ConvertMilliToTime(timediff));
			
			ret.put("response", resp);
			
		}else if(action.equals("findtxpow")) {
			
			JSONObject resp = new JSONObject();
			
			if(existsParam("txpowid")) {
				String txpowid = getParam("txpowid");
				TxPoW txp = mysql.getTxPoW(txpowid);
				
				if(txp == null) {
					
					//Could be a block
					TxBlock txblk = mysql.loadBlockFromID(txpowid);
					
					if(txblk == null) {
						resp.put("found", false);
					}else {
						resp.put("found", true);
						resp.put("txpow", txblk.getTxPoW().toJSON());
					}
					
				}else {
					resp.put("found", true);
					resp.put("txpow", txp.toJSON());
				}
			}else if(existsParam("block")) {
				
				MiniNumber block = getNumberParam("block");
				
				TxBlock txp = mysql.loadBlockFromNum(block.getAsLong());
				
				if(txp == null) {
					resp.put("found", false);
				}else {
					resp.put("found", true);
					resp.put("txpow", txp.getTxPoW().toJSON());
				}
				
			}else {
				throw new CommandException("MUST provide either txpowid or block for findtxpow function");
			}
			
			ret.put("response", resp);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		//Then shutdown
		mysql.shutdown();
		
		return ret;
	}
	
	/**
	 * Convert the mysql param from command line
	 */
	public static void convertMySQLParams(String zMySQLDB) {
		
		try {
			
			int breaker = zMySQLDB.indexOf("@");
			
			String user = zMySQLDB.substring(0,breaker);
			String db 	= zMySQLDB.substring(breaker+1,zMySQLDB.length());
			
			//Now chop up the User
			int userbreak   = user.indexOf(":");
			String username = user.substring(0,userbreak);
			String password = user.substring(userbreak+1,user.length());
			
			//And the DB
			int dbslash   = db.indexOf("/");
			String dbhost = db.substring(0,dbslash);
			String dbname = db.substring(dbslash+1,db.length());
			
			MinimaLogger.log("MYSQL Database Setup : "+username+":***@"+dbhost+" / "+dbname);
			
			UserDB udb = MinimaDB.getDB().getUserDB();
			
			//Store everything..
			GeneralParams.MYSQL_STORE_ALLTXPOW = true;
			
			udb.setAutoMySQLHost(dbhost);
			udb.setAutoMySQLDB(dbname);
			
			udb.setAutoMySQLUser(username);
			udb.setAutoMySQLPassword(password);
			
			udb.setAutoLoginDetailsMySQL(true);
			
			//And Auto Backup everything..
			udb.setAutoBackupMySQL(true);
			
			//Try and Connect to the DB..
			if(GeneralParams.MYSQL_DB_DELAY != 0) {
				MinimaLogger.log("Waiting "+GeneralParams.MYSQL_DB_DELAY+" ms before attempting first MySQL connection..");
				Thread.sleep(GeneralParams.MYSQL_DB_DELAY);
			}
			
			//NOW attempt a connection
			MySQLConnect mysql 	= new MySQLConnect(dbhost, dbname, username, password, true);
			mysql.init();
			mysql.shutdown();
			
		}catch(Exception exc) {
			
			//Failed..
			MinimaLogger.log("Failed to connect to MySQL DB - MUST be username:password@host:port/database "+exc.toString());
		}
	}
	
	@Override
	public Command getFunction() {
		return new mysql();
	}	
	
	public static void main(String[] zArgs) {
		
		MySQLConnect conn = new MySQLConnect("127.0.0.1:3306", "minimadb", "minimauser", "minimapassword");
		try {
			System.out.println("Attemp Connect!");
			conn .init();
			
			System.out.println("Connected!");
			
			int count = conn.getCount();
			System.out.println("Rows : "+count);
			
			
			conn.shutdown();
			
			System.out.println("Shutdown!");
			
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
	}
}
