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

public class minidappsync extends Command {

	public minidappsync() {
		super("minidappsync","Sync a raw archive file and address to a MiniDAPP via MDS_RESYNC calls..");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","file","address","minidappuid"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//What action are we doing..
		String action = getParam("action","info");
		
		boolean logs = true;
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		//if(action.equals("rawimport")) {
			
			long timestart = System.currentTimeMillis();
			
			//Get the MiniDAPP and address you are interested in..
			String id 		= getParam("minidappuid");
			String address 	= getParam("address");
			
			//Get the file
//			String infile 	= getParam("file");
//			File fileinfile = MiniFile.createBaseFile(infile);
//			RawArchiveInput rawin = new RawArchiveInput(fileinfile);
//			rawin.connect();
			
			//Tell the MiniDAPP we are starting..
			sendMessage(id, createSimpleMessage("SYNC_START",address));
			
			//Load a range..
			/*long endblock 	= -1;
			TxBlock lastblock = null;
			int counter = 0;
			while(true) {
				//Get the next batch of data..
				IBD syncibd 				= rawin.getNextIBD();
				ArrayList<TxBlock> blocks 	= syncibd.getTxBlocks();
				
				if(logs) {
					if(counter % 10 ==0) {
						if(blocks.size()>0) {
							MinimaLogger.log("Loading from RAW Block : "+blocks.get(0).getTxPoW().getBlockNumber(),false);
						}
					}
				}
				
				if(blocks.size()==0) {
					//All blocks checked
					break;
				}
				
				//Cycle and add to our DB..
				for(TxBlock block : blocks) {
					
					//Send to the MiniDAPP
					//mysql.saveBlock(block);
					//MinimaLogger.log("Save block : "+block.getTxPoW().getBlockNumber());
					
					//Store for later
					lastblock = block;
				}
				
				//Clean up..
				counter++;
				if(counter % 10 == 0) {
					System.gc();
				}
			}*/
			
			
			//Shutdown TEMP DB
			//rawin.stop();
			
			long timediff = System.currentTimeMillis() - timestart;
			
			//Now load all the final blocks from the archive db itself..
			MiniNumber firstblock = MiniNumber.ZERO;
			
			//Loop through the archive db
			while(true) {
				
				//Get a list of blocks
				ArrayList<TxBlock> mysqlblocks = arch.loadBlockRange(firstblock, firstblock.add(MiniNumber.HUNDRED), false);
				if(mysqlblocks.size()==0) {
					//No blocks left
					break;
				}
				
				for(TxBlock block : mysqlblocks) {
					
					//Created
					ArrayList<Coin> outputs 		= block.getOutputCoins();
					for(Coin cc : outputs) {
						if(cc.getAddress().to0xString().equals(address)) {
							//Found a coin..
							cc.setBlockCreated(block.getTxPoW().getBlockNumber());
							sendMessage(id, createCoinMessage(block,cc,false));
						}
					}
					
					//Start from here next time..
					firstblock = block.getTxPoW().getBlockNumber(); 
				}
			}
			
			//And it's finished..
			sendMessage(id, createSimpleMessage("SYNC_END",address));
			
			JSONObject resp = new JSONObject();
			resp.put("time", MiniFormat.ConvertMilliToTime(timediff));
			
			ret.put("response", resp);
			
		/*}else {
			throw new CommandException("Invalid action : "+action);
		}*/
		
		return ret;
	}
	
	public void sendMessage(String zMiniDAPP, JSONObject zData) {
		Main.getInstance().PostNotifyEvent("MDS_RESYNC", zData, zMiniDAPP);
	}
	
	public JSONObject createSimpleMessage(String zMessage, String zAddress) {
		JSONObject json = new JSONObject();
		json.put("type", "simple");
		json.put("address", zAddress);
		json.put("message", zMessage);
		return json;
	}
	
	public JSONObject createCoinMessage(TxBlock zBlock, Coin zCoin, boolean zSpent) {
		JSONObject json = new JSONObject();
		json.put("type", "coin");
		json.put("address", zCoin.getAddress().to0xString());
		json.put("txblockid", zBlock.getTxPoW().getTxPoWID());
		json.put("txblock", zBlock.getTxPoW().getBlockNumber().toString());
		json.put("spent", zSpent);
		json.put("coin", zCoin.toJSON(true));
		
		return json;
	}
	
	@Override
	public Command getFunction() {
		return new minidappsync();
	}	
}
