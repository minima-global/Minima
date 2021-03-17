package org.minima.system.brains;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.userdb.java.JavaUserDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.SyncPacket;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusBackup extends ConsensusProcessor {

	public static final String CONSENSUS_PREFIX  = "CONSENSUSBACKUP_";
	
	public static String CONSENSUSBACKUP_BACKUPUSER  = CONSENSUS_PREFIX+"BACKUPUSER"; 
	
	public static String CONSENSUSBACKUP_SYNCBACKUP   = CONSENSUS_PREFIX+"SYNCBACKUP"; 
	public static String CONSENSUSBACKUP_SYNCRESTORE  = CONSENSUS_PREFIX+"SYNCRESTORE"; 
	
	public static String CONSENSUSBACKUP_BACKUP  = CONSENSUS_PREFIX+"BACKUP"; 
	
	public static String CONSENSUSBACKUP_RESTORE        = CONSENSUS_PREFIX+"RESTORE"; 
	public static String CONSENSUSBACKUP_RESTOREUSERDB  = CONSENSUS_PREFIX+"RESTOREUSERDB"; 
//	public static String CONSENSUSBACKUP_RESTORETXPOW   = CONSENSUS_PREFIX+"RESTORETXPOW"; 
//	public static String CONSENSUSBACKUP_RESTORETREEDB  = CONSENSUS_PREFIX+"RESTORETREEDB"; 
	
	public static String CONSENSUSBACKUP_RESET        	= CONSENSUS_PREFIX+"RESET"; 
	
	
	public static final String USERDB_BACKUP = "user.minima";
	public static final String SYNC_BACKUP   = "sync.package";
	
	public ConsensusBackup(MinimaDB zDB, ConsensusHandler zHandler) {
		super(zDB, zHandler);
	}
	
	private BackupManager getBackup() {
		return Main.getMainHandler().getBackupManager();
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUSBACKUP_BACKUPUSER)) {
			//Get this as will need it a few times..
			BackupManager backup = getBackup();
			
			//First backup the UserDB..
			JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
			File backuser     = backup.getBackUpFile(USERDB_BACKUP);
			MiniFile.writeObjectToFile(backuser, userdb);
	
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_SYNCBACKUP)) {
			//Return details..
			JSONObject details = InputHandler.getResponseJSON(zMessage);
			
			//Get the file..
			String file = zMessage.getString("file");
			
			//Create the output file - and delete if exists
			File fullbackup = new File(file);
			if(fullbackup.exists()) {
				fullbackup.delete();
			}
			
			//Add to the returned details
			details.put("file", fullbackup.getAbsolutePath());
			
			//First backup the UserDB..
			try {
				JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
				MiniFile.writeObjectToFile(fullbackup, userdb);
				
				//Now the complete SyncPackage..
				SyncPackage sp = getMainDB().getSyncPackage();
				MiniFile.writeObjectToFile(fullbackup, sp, true);
				
			}catch(Exception exc) {
				MinimaLogger.log(exc);
				details.put("error", exc.toString());
				InputHandler.endResponse(zMessage, false, "Backup Error");
				return;
			}
			
			//Reset..
			fullbackup = new File(file);
			details.put("size", MiniFormat.formatSize(fullbackup.length()));
			
			//Where
			InputHandler.endResponse(zMessage, true, "Full Backup Performed");
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_BACKUP)) {
			//Return details..
			JSONObject details = InputHandler.getResponseJSON(zMessage);
			
			//Is this for shut down or just a regular backup..
			boolean shutdown = false;
			if(zMessage.exists("shutdown")) {
				shutdown = zMessage.getBoolean("shutdown");
			}
			
			//Get this as will need it a few times..
			BackupManager backup = getBackup();
			
			//First backup the UserDB..
			try {
				JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
				File backuser     = backup.getBackUpFile(USERDB_BACKUP);
				MiniFile.writeObjectToFile(backuser, userdb);
				details.put("userdb", backuser.getAbsolutePath());
				
				//Now the complete SyncPackage..
				SyncPackage sp = getMainDB().getSyncPackage();
				File backsync  = backup.getBackUpFile(SYNC_BACKUP);
				MiniFile.writeObjectToFile(backsync, sp);
				details.put("chaindb", backsync.getAbsolutePath());
				
			}catch(Exception exc) {
				MinimaLogger.log("BACKUP ERROR : ");
				exc.printStackTrace();
			}
			
			//Do we shut down..
			if(shutdown) {
				Message fullshut = new Message(Main.SYSTEM_FULLSHUTDOWN);
				InputHandler.addResponseMesage(fullshut, zMessage);
				Main.getMainHandler().PostMessage(fullshut);
				MinimaLogger.log("Backup on shutdown fininshed..");
			}else {
				//respond..
				InputHandler.endResponse(zMessage, true, "Full Backup Performed");	
			}
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORE)) {
			MinimaLogger.log("Begin Restore..");
			
			//Get this as will need it a few times..
			BackupManager backup = getBackup();
			
			//Check the backups exist..
			File backuser  = backup.getBackUpFile(USERDB_BACKUP);
			File backsync  = backup.getBackUpFile(SYNC_BACKUP);
			
			//Are we ok ?
			if(!backuser.exists()) {
				//Not OK.. start fresh.. 
				MinimaLogger.log("No User backups found.. @ "+backuser.getAbsolutePath());
				Main.getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
			
			if(!backsync.exists()) {
				//Not OK.. start fresh.. 
				MinimaLogger.log("No SyncPackage found.. @ "+backsync.getAbsolutePath());
				Main.getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
			
			//Load the user..
			getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Loading User DB"));
			
			JavaUserDB jdb = new JavaUserDB();
			try {
				//Load the file into memory first - FAST
				byte[] userdb = MiniFile.readCompleteFile(backuser);
				ByteArrayInputStream bais = new ByteArrayInputStream(userdb);
				DataInputStream dis = new DataInputStream(bais);
				
				jdb.readDataStream(dis);
				dis.close();
				bais.close();
			}catch (Exception exc) {
				exc.printStackTrace();
				//HMM.. not good.. file corrupted.. bug out
				getConsensusHandler().updateListeners(
						new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "USER BACKUP FILE CORRUPTED.. not starting up.. :("));
				MinimaLogger.log("USER BACKUP FILE CORRUPTED.. not starting up.. :(");
				return;
			}
			
			//Set it..
			getMainDB().setUserDB(jdb);
			
			//Load the SyncPackage
			getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Loading MMR DB"));
			
			MinimaLogger.log("Loading DB.. please wait..");
			SyncPackage sp = new SyncPackage();
			try {
				byte[] chaindb = MiniFile.readCompleteFile(backsync);
				ByteArrayInputStream bais = new ByteArrayInputStream(chaindb);
				DataInputStream dis = new DataInputStream(bais);
				
				sp.readDataStream(dis);
				dis.close();
				bais.close();
			}catch(Exception exc) {
				exc.printStackTrace();
				//HMM.. not good.. file corrupted.. bug out
				getConsensusHandler().updateListeners(
						new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "SYNCPACKAGE MMR BACKUP FILE CORRUPTED.. not starting up.. :("));
				MinimaLogger.log("SYNCPACKAGE MMR BACKUP FILE CORRUPTED.. not starting up.. :(");
				return;
			}
			
			//And now load it..
			loadSyncPackage(sp);
			
			//We now have ALL the TxPoW units loaded..
			//Would be nice to clear the folder and save them ALL..
			//TODO..
			//..
			
			//Get on with it..
			Main.getMainHandler().PostMessage(Main.SYSTEM_INIT);
		
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_SYNCRESTORE)) {
			//Get the file..
			String file = zMessage.getString("file");
			
			//Return details..
			JSONObject details = InputHandler.getResponseJSON(zMessage);
			
			//Create the output file - and delete if exists
			File fullrestore = new File(file);
			
			//Add to the returned details
			details.put("file", fullrestore.getAbsolutePath());

			if(!fullrestore.exists() || fullrestore.isDirectory()) {
				InputHandler.endResponse(zMessage, false, "Restore file does not exist");
				return;
			}
			
			//Load the whole file
			byte[] restore 				= MiniFile.readCompleteFile(fullrestore);
			if(restore.length == 0) {
				InputHandler.endResponse(zMessage, false, "Error loading restore file..");
				return;
			}
			
			//OK - LETS DO IT..
			
			//Clear the database..
			getMainDB().getMainTree().clearTree();
//			getMainDB().getCoinDB().clearDB();
			getMainDB().getTxPowDB().ClearDB();
			
			//Wipe everything BUT the minidapp folder
			BackupManager.deleteConfFolder(getBackup().getRootFolder());
			
			//Load the whole file
			ByteArrayInputStream bais 	= new ByteArrayInputStream(restore);
			DataInputStream dis 		= new DataInputStream(bais);
			
			//First the UserDB
			JavaUserDB jdb = new JavaUserDB();
			jdb.readDataStream(dis);
				
			//Set it..
			getMainDB().setUserDB(jdb);
			
			//And now..
			SyncPackage sp = new SyncPackage();
			sp.readDataStream(dis);
			
			//All done..
			dis.close();
			bais.close();
			
			//And now load it..
			loadSyncPackage(sp);
			
			//Disconnect and Reconnect to the network..
			getNetworkHandler().PostMessage(NetworkHandler.NETWORK_RECONNECT);
		
			//Message
			InputHandler.endResponse(zMessage, true, "Restore complete - reconnecting to network");
		
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESET)) {
			//Return details..
			JSONObject details = InputHandler.getResponseJSON(zMessage);
			
			MinimaLogger.log("RESETTING MINIMA..");
			
			//Clear the database..
			getMainDB().getMainTree().clearTree();
//			getMainDB().getCoinDB().clearDB();
			getMainDB().getTxPowDB().ClearDB();
			
			//Wipe everything BUT the minidapp folder
			BackupManager.deleteConfFolder(getBackup().getRootFolder());
			
			//Disconnect and Reconnect to the network..
			getNetworkHandler().PostMessage(NetworkHandler.NETWORK_RECONNECT);
		
			//Message
			InputHandler.endResponse(zMessage, true, "FULL RESET complete - reconnecting to network");
		}
		
	}
	
	public void loadSyncPackage(SyncPackage zPackage) {
		//Get the SyncPackage
		MiniNumber casc = zPackage.getCascadeNode();
		
		//Drill down
		ArrayList<SyncPacket> packets = zPackage.getAllNodes();
		
		float syncsize = packets.size();
		float tot = 0;
		for(SyncPacket spack : packets) {
			//Print some stuff..
			int curr  = (int)( (tot++ / syncsize) *100);
			getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Checking DB.."+curr+"%"));
			
			TxPoW txpow     = spack.getTxPOW();
			MMRSet mmrset   = spack.getMMRSet();
			boolean cascade = spack.isCascade();
			
			//Check all MMR in the unbroken chain.. no point in cascade as may have changed..
			if(mmrset!=null) {
				if(mmrset.getBlockTime().isMoreEqual(casc)) {
					getMainDB().scanMMRSetForCoins(mmrset);
				}
			}
			
			//Add it to the DB..
			BlockTreeNode node = getMainDB().hardAddTxPOWBlock(txpow, mmrset, cascade);
		
			//Load the TxPOW files in the block..
			BackupManager backup = getBackup();
			ArrayList<MiniData> txns = txpow.getBlockTransactions();
			for(MiniData txn : txns) {
				TxPoW txinblock = loadTxPOW(backup.getTxpowFile(txn));
				
				//Add it..
				if(txinblock != null) {
					getMainDB().addNewTxPow(txinblock);	
				}
			}
		
			//Is this the cascade block
			if(txpow.getBlockNumber().isEqual(zPackage.getCascadeNode())) {
				getMainDB().hardSetCascadeNode(node);
			}
			
			//Store it..
			getBackup().backupTxpow(txpow);
		}
		getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Checking DB..100%"));
		MinimaLogger.log("Checking DB.. 100%");
		
		//Reset weights
		getMainDB().getMainTree().resetWeights();
		
		//And Now sort the TXPOWDB
		ArrayList<BlockTreeNode> list = getMainDB().getMainTree().getAsList();
		getMainDB().getTxPowDB().resetAllInBlocks();
		
		//Now sort
		syncsize = list.size();
		tot = 0;
		for(BlockTreeNode treenode : list) {
			//Print some stuff..
			int curr   = (int)( (tot++/syncsize) *100);
			getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Restoring DB.."+curr+"%"));
			
			//Get the Block
			TxPoW txpow = treenode.getTxPow();
			
			//What Block
			MiniNumber block = txpow.getBlockNumber();
			
			//Set the main chain details..
			TxPOWDBRow blockrow = getMainDB().getTxPowDB().findTxPOWDBRow(txpow.getTxPowID());
			blockrow.setInBlockNumber(block);
			blockrow.setMainChainBlock(true);
			blockrow.setIsInBlock(true);
			
			//Now the Txns..
			ArrayList<MiniData> txpowlist = txpow.getBlockTransactions();
			for(MiniData txid : txpowlist) {
				TxPOWDBRow trow = getMainDB().getTxPowDB().findTxPOWDBRow(txid);
				if(trow!=null) {
					//Set that it is in this block
					trow.setMainChainBlock(false);
					trow.setIsInBlock(true);
					trow.setInBlockNumber(block);
					
					//Is it a block ?
					TxPoW tpow = trow.getTxPOW();
					if(tpow.isBlock()) {
						//Add all the children
						if(getMainDB().getMainTree().addNode(new BlockTreeNode(tpow),true)) {
							getMainDB().addTreeChildren(tpow.getTxPowID());
						}
					}
				}else {
					MinimaLogger.log("MISSING "+txid.to0xString());
				}
			}
		}
				
		ArrayList<TxPOWDBRow> test = getMainDB().getTxPowDB().getAllUnusedTxPOW();
		if(test.size()>0) {
			MinimaLogger.log("UNUSED TXPOW FOUND "+test.size());
			for(TxPOWDBRow row : test) {
				MinimaLogger.log(row.getTxPOW().getTxPowID().to0xString());
			}
		}
		
		//MinimaLogger.log("DB.. 100%");
		getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Restoring DB..100%"));
	}
	
	public static TxPoW loadTxPOW(File zTxpowFile) {
		if(!zTxpowFile.exists()) {
			//MinimaLogger.log("Load TxPOW Doesn't exist! "+zTxpowFile.getName());
			return null;
		}
		
		//The TxPOW File..
		TxPoW txpow    = new TxPoW();
		
		try {
			//Load the complete file first..
			byte[] txfile = MiniFile.readCompleteFile(zTxpowFile);
			
			//Now load from memory..
			ByteArrayInputStream bais = new ByteArrayInputStream(txfile);
			DataInputStream dis = new DataInputStream(bais);
			txpow.readDataStream(dis);
			dis.close();
			bais.close();
			
		} catch (Exception e) {
			MinimaLogger.log("ERROR loading TxPOW "+zTxpowFile.getName());
			
			//Delete it..
			zTxpowFile.delete();
			
			return null;
		}
		
		return txpow;
	}
	
}
