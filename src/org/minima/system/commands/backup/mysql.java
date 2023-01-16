package org.minima.system.commands.backup;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.Wallet;
import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.network.connect;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.utils.BIP39;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;
import org.minima.utils.mysql.MySQLConnect;

public class mysql extends Command {

	public mysql() {
		super("mysql","");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","database","user","password"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the details
		String host 		= getParam("host");
		String db 			= getParam("database");
		String user 		= getParam("user");
		String password 	= getParam("password");
		
		//Get the login details..
		MySQLConnect mysql = new MySQLConnect(host, db, user, password);
		mysql.init();
		
		String action = getParam("action","info");
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		if(action.equals("info")) {
			
			//Check the DB
			long firstblock = mysql.loadFirstBlock();
			long lastblock 	= mysql.loadLastBlock();
			long total 		= firstblock-lastblock; 
			
			TxBlock archlastblock 	= arch.loadLastBlock();
			TxBlock archfirstblock	= arch.loadFirstBlock();
			long archtotal 			= archfirstblock.getTxPoW().getBlockNumber().sub(archlastblock.getTxPoW().getBlockNumber()).getAsLong();
			
			JSONObject mysqlresp = new JSONObject();
			mysqlresp.put("mysqlstart", lastblock);
			mysqlresp.put("mysqlend", firstblock);
			mysqlresp.put("mysqltotal", total);
			
			JSONObject archresp = new JSONObject();
			archresp.put("archivestart", archlastblock.getTxPoW().getBlockNumber().getAsLong());
			archresp.put("archiveend", archfirstblock.getTxPoW().getBlockNumber().getAsLong());
			archresp.put("archivetotal", archtotal);
			
			JSONObject resp = new JSONObject();
			resp.put("mysql", mysqlresp);
			resp.put("archive", archresp);
			
			ret.put("response", resp);
		
		}else if(action.equals("wipe")) {
			
			//First wipe the DB
			mysql.wipeAll();
			
			ret.put("response", "MySQL DB Wiped..");
		
		}else if(action.equals("integrity")) {
			
			//Check all the block in the MySQL DB..
			long mysqllastblock 	= mysql.loadLastBlock();
			long mysqlfirstblock 	= mysql.loadFirstBlock();
			
			
		}else if(action.equals("update")) {
			
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
			while(!finished){
			
				long ender = startload+50;
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
					
					long saveblock = block.getTxPoW().getBlockNumber().getAsLong();
					//MinimaLogger.log("SAVING to MySQL : "+saveblock);
				
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
			long lastblock 		= mysql.loadLastBlock();
			long total 			= firstblock-lastblock; 
			
			TxBlock archlastblock 	= arch.loadLastBlock();
			TxBlock archfirstblock	= arch.loadFirstBlock();
			long archtotal 			= archfirstblock.getTxPoW().getBlockNumber().sub(archlastblock.getTxPoW().getBlockNumber()).getAsLong();
			
			JSONObject mysqlresp = new JSONObject();
			mysqlresp.put("mysqlstart", lastblock);
			mysqlresp.put("mysqlend", firstblock);
			mysqlresp.put("mysqltotal", total);
			
			JSONObject archresp = new JSONObject();
			archresp.put("archivestart", archlastblock.getTxPoW().getBlockNumber().getAsLong());
			archresp.put("archiveend", archfirstblock.getTxPoW().getBlockNumber().getAsLong());
			archresp.put("archivetotal", archtotal);
			
			JSONObject resp = new JSONObject();
			resp.put("mysql", mysqlresp);
			resp.put("archive", archresp);
			
			ret.put("response", resp);
			
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
