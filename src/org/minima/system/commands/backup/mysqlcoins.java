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
				+ "Export the archive data of this node to a MySQL d.\n"
				+ "\n"
				+ "The MySQL db can be used to perform a chain re-sync to put users on the correct chain,\n"
				+ "\n"
				+ "or a seed re-sync to restore access to lost funds, using the seed phrase.\n"
				+ "\n"
				+ "Can query an address for its history of spent and unspent coins.\n"
				+ "\n"
				+ "Additionally export the MySQL db to a gzip file for resyncing with 'reset' or 'archive' command.\n"
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
				+ "    autobackup : Automatically save archive data to MySQL DB. Use with enable.\n"
				+ "    resync : Perform a chain or seed re-sync from the specified MySQL db.\n"
				+ "             Will shutdown the node so you must restart it once complete.\n"
				+ "    wipe :  Be careful. Wipe the MySQL db.\n"
				+ "    h2export : export the MySQL db to an archive gzip file which can be used to resync a node.\n"
				+ "    h2import : import an archive gzip file to the MySQL db.\n"
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
				+ "\n"
				+ "mysql host:mysqlhost:port database:archivedb user:archiveuser password:archivepassword action:h2export file:archivexport-DDMMYY.gzip\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","database","user","password","keys","keyuses","phrase","address","enable","file","statecheck","logs","maxexport","readonly"}));
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
			
			long startload 	= mysqllastblock;
			int counter = 0;
			while(true) {
				
				ArrayList<TxBlock> blocks = mysql.loadBlockRange(new MiniNumber(startload));
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Small log message
				if(counter % 20 == 0) {
					System.gc();
					if(logs) {
						MinimaLogger.log("Loading from MySQL @ "+startload+" Blocks:"+blocks.size());
					}
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					MiniNumber blocknum = block.getTxPoW().getBlockNumber();
					
					//Get the input coins
					ArrayList<CoinProof> inputs = block.getInputCoinProofs();
					for(CoinProof cc : inputs) {
						MinimaLogger.log("Found Input coin @ "+blocknum+" : "+cc.getCoin().getCoinID());
						cc.getCoin().setSpent(true);
						mysql.insertCoin(cc.getCoin());
					}
					
					//Get the output coins
					ArrayList<Coin> outputs = block.getOutputCoins();
					for(Coin cc : outputs) {
						MinimaLogger.log("Found Output coin @ "+blocknum+" : "+cc.getCoinID());
						cc.setSpent(false);
						mysql.insertCoin(cc);
					}
					
					if(lastblock == null) {
						firstblock = block.getTxPoW().getBlockNumber().getAsLong();
					}
					lastblock = block;
					endblock = block.getTxPoW().getBlockNumber().getAsLong();
					
					//Increase counter
					counter++;
				}
				
				startload = endblock+1;
			}
			
			ret.put("response", "MySQL CoinsDB Created..");
					
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
