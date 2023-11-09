package org.minima.system.commands.backup;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
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

public class mysqlcoins extends Command {

	public mysqlcoins() {
		super("mysqlcoins","Create and search a coins database from your MySQL Archive");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmysqlcoins\n"
				+ "\n"
				+ "Create a coins db from your mysql data and search it.\n"
				+ "\n"
				+ "Use the same database you already use for your TxBlocks. Just creates a new table.\n"
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
				+ "query:\n"
				+ "    The search criteria. String data MUST be in single quotes. You can use multiple parameters.\n"
				+ "\n"
				+ "action:\n"
				+ "    createdb : Take the MySQL data and make a coins database. Takes a while. Logs output to stdout.\n"
				+ "    search : Perform a search on the data. You can specify any valid query params.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "mysqlcoins host:127.0.0.1:3306 database:coinsdb user:myuser password:myuser action:search query:\"address='0x791E78C60652B0E19B8FE9EB035B122B261490C477FD76E38C0C928187076103'\"\n"
				+ "\n"
				+ "mysqlcoins host:127.0.0.1:3306 database:coinsdb user:myuser password:myuser action:search query:\"address='0x791E78C60652B0E19B8FE9EB035B122B261490C477FD76E38C0C928187076103' AND state LIKE '0xFFEEDD' \"\n"
				;
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","database","user","password","logs","readonly","query"}));
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
		
		boolean logs		= getBooleanParam("logs", true);
		
		//Get the login details..
		boolean readonly 	= getBooleanParam("readonly", false);
		MySQLConnect mysql 	= new MySQLConnect(host, db, user, password, readonly);
		mysql.init();
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		if(action.equals("info")) {
			
			
		}else if(action.equals("createdb")) {
			
			MinimaLogger.log("Creating CoinsDB..");
			
			//Wipe the Old db
			mysql.wipeCoinsDB();
			
			//Load the MySQL and output to the H2
			long mysqllastblock 	= mysql.loadLastBlock();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			//Load a range..
			long firstblock = -1;
			long endblock 	= -1;
			TxBlock lastblock = null;
			
			long startload 		= mysqllastblock;
			int counter 		= 0;
			int coincounter 	= 0;
			int blockcounter 	= 0;
			long starttimer = System.currentTimeMillis();
			long timer	 	= starttimer;
			while(true) {
				
				ArrayList<TxBlock> blocks = mysql.loadBlockRange(new MiniNumber(startload));
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Do we show a message
				if(logs) {
					long timenow = System.currentTimeMillis();
					if(timenow - timer > 5000) {
						timer = timenow;
						MinimaLogger.log("Loading from MySQL @ "+startload);
					}
				}
				
				//Small log message
				if(counter % 10 == 0) {
					System.gc();
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					blockcounter++;
					
					MiniNumber blocknum = block.getTxPoW().getBlockNumber();
					
					//Get the input coins
					ArrayList<CoinProof> inputs = block.getInputCoinProofs();
					for(CoinProof cc : inputs) {
						//MinimaLogger.log("Found Input coin @ "+blocknum+" : "+cc.getCoin().getCoinID());
						cc.getCoin().setSpent(true);
						mysql.insertCoin(cc.getCoin());
						coincounter++;
					}
					
					//Get the output coins
					ArrayList<Coin> outputs = block.getOutputCoins();
					for(Coin cc : outputs) {
						//MinimaLogger.log("Found Output coin @ "+blocknum+" : "+cc.getCoinID());
						cc.setSpent(false);
						cc.setBlockCreated(blocknum);
						mysql.insertCoin(cc);
						coincounter++;
					}
					
					if(lastblock == null) {
						firstblock = block.getTxPoW().getBlockNumber().getAsLong();
					}
					lastblock = block;
					endblock = block.getTxPoW().getBlockNumber().getAsLong();
				}
				
				//Increase counter
				counter++;
				
				startload = endblock+1;
			}
			
			long endtimer = System.currentTimeMillis();
			long timediff = endtimer - starttimer;
			
			JSONObject resp = new JSONObject();
			resp.put("duration", timediff);
			resp.put("blocks", blockcounter);
			resp.put("coinsadded", coincounter);
			resp.put("message", "MySQL CoinsDB Created..");
			ret.put("response", resp);
					
		}else if(action.equals("search")) {
			
			//Get the generic query
			String query = getParam("query");
			
			//Run it..
			JSONObject res = mysql.searchCoins(query);
			
			//Return the results..
			ret.put("response", res);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		//Then shutdown
		mysql.shutdown();
		
		return ret;
	}
	
	
	@Override
	public Command getFunction() {
		return new mysqlcoins();
	}	
}
