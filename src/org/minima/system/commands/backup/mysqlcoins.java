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
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","database","user","password","logs","readonly","query","where","maxblocks","maxcoins","hidetoken"}));
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
		
		boolean logs	= getBooleanParam("logs", false);
		int maxblocks	= getNumberParam("maxblocks", MiniNumber.BILLION).getAsInt();
		int maxcoins	= getNumberParam("maxcoins", MiniNumber.BILLION).getAsInt();
		
		//Get the login details..
		boolean readonly 	= getBooleanParam("readonly", false);
		MySQLConnect mysql 	= new MySQLConnect(host, db, user, password, readonly);
		mysql.init();
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		if(action.equals("info")) {
			
			long total = mysql.getTotalCoins();
			long lastblock = mysql.getMaxCoinBlock();
			
			JSONObject resp = new JSONObject();
			resp.put("lastblock", lastblock);
			resp.put("total", total);
			ret.put("response", resp);
		
		}else if(action.equals("wipe")) {
			
			MinimaLogger.log("Wiping CoinsDB..");
			
			//Wipe the Old db
			mysql.wipeCoinsDB();
			
			ret.put("respnse", "CoinsDB wiped");
			
		}else if(action.equals("update")) {
			
			MinimaLogger.log("Updating CoinsDB..");
			
			//Load the start of the mysql
			long mysqlstartblock 	= mysql.loadLastBlock();
			
			//Load the last coin added...
			long lastcoin = mysql.getMaxCoinBlock();
			if(lastcoin != -1) {
				MinimaLogger.log("Last coin found @ "+lastcoin);
				mysqlstartblock = lastcoin+1;
			}
			MinimaLogger.log("Starting from : "+mysqlstartblock);
			
			//Load a range..
			long firstblock = -1;
			long endblock 	= -1;
			
			long startload 		= mysqlstartblock;
			int counter 		= 0;
			int coincounter 	= 0;
			int blockcounter 	= 0;
			long starttimer = System.currentTimeMillis();
			long timer	 	= starttimer;
			boolean maxblocksreached = false;
			while(true && !maxblocksreached) {
				
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
					
					//Check maxblocks
					if(blockcounter>maxblocks){
						blockcounter--;
						maxblocksreached = true;
						break;
					}
					
					MiniNumber blocknum = block.getTxPoW().getBlockNumber();
					if(firstblock == -1) {
						firstblock = blocknum.getAsLong();
					}
					
					//Get the input coins
					ArrayList<CoinProof> inputs = block.getInputCoinProofs();
					for(CoinProof cc : inputs) {
						if(logs) {
							MinimaLogger.log("Found Input coin @ "+blocknum+" : "+cc.getCoin().toJSON().toString());
						}
						cc.getCoin().setSpent(true);
						mysql.insertCoin(cc.getCoin());
						coincounter++;
					}
					
					//Get the output coins
					ArrayList<Coin> outputs = block.getOutputCoins();
					for(Coin cc : outputs) {
						if(logs) {
							MinimaLogger.log("Found Output coin @ "+blocknum+" : "+cc.toJSON().toString());
						}
						cc.setSpent(false);
						cc.setBlockCreated(blocknum);
						mysql.insertCoin(cc);
						coincounter++;
					}
					
					//Last block
					endblock  = block.getTxPoW().getBlockNumber().getAsLong();
					
					//Have we reached the max coins..
					if(coincounter>maxcoins) {
						MinimaLogger.log("Max coins added : "+coincounter+"/"+maxcoins);
						maxblocksreached=true;
						break;
					}
				}
				
				//Increase counter
				counter++;
				
				startload = endblock+1;
			}
			
			long endtimer = System.currentTimeMillis();
			long timediff = endtimer - starttimer;
			
			JSONObject resp = new JSONObject();
			resp.put("duration", (timediff/1000)+" secconds");
			resp.put("blocks", blockcounter);
			resp.put("firstblock", firstblock);
			resp.put("lastblock", endblock);
			resp.put("coinsadded", coincounter);
			resp.put("message", "MySQL CoinsDB Updated..");
			ret.put("response", resp);
					
		}else if(action.equals("search")) {
			
			//Get the generic query
			String sql = null;
			if(existsParam("where")) {
				String where = getParam("where");
				sql = "SELECT * FROM coins WHERE "+where; 
			
			}else if(existsParam("query")) {
				sql = getParam("query"); 
				
			}else {
				throw new CommandException("MUST provide where or query parameter..");
			}
			
			//So we show token details - can be VERY large
			boolean hidetoken = getBooleanParam("hidetoken", false);
			
			//Run it..
			if(logs) {
				MinimaLogger.log("Start Coins query : "+sql);
			}
			long timestartquery = System.currentTimeMillis();
			JSONObject res = mysql.searchCoins(sql,hidetoken);
			long timeendquery = System.currentTimeMillis();
			if(logs) {
				MinimaLogger.log("Finished query : "+sql);
				MinimaLogger.log("Rows : "+res.get("count"));
			}
			
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
