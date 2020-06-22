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
import org.minima.system.Main;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.system.input.InputHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusBackup extends ConsensusProcessor {

	public static final String CONSENSUS_PREFIX  = "CONSENSUSBACKUP_";
	
	public static String CONSENSUSBACKUP_BACKUPUSER  = CONSENSUS_PREFIX+"BACKUPUSER"; 
	
	public static String CONSENSUSBACKUP_BACKUP  = CONSENSUS_PREFIX+"BACKUP"; 
	
	public static String CONSENSUSBACKUP_RESTORE        = CONSENSUS_PREFIX+"RESTORE"; 
	public static String CONSENSUSBACKUP_RESTOREUSERDB  = CONSENSUS_PREFIX+"RESTOREUSERDB"; 
	public static String CONSENSUSBACKUP_RESTORETXPOW   = CONSENSUS_PREFIX+"RESTORETXPOW"; 
	public static String CONSENSUSBACKUP_RESTORETREEDB  = CONSENSUS_PREFIX+"RESTORETREEDB"; 
	
	public static final String USERDB_BACKUP = "user.minima";
	public static final String SYNC_BACKUP   = "sync.package";
	
	public ConsensusBackup(MinimaDB zDB, ConsensusHandler zHandler) {
		super(zDB, zHandler);
	}
	
	private BackupManager getBackup() {
		return getConsensusHandler().getMainHandler().getBackupManager();
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUSBACKUP_BACKUPUSER)) {
			//Get this as will need it a few times..
			BackupManager backup = getBackup();
			
			//First backup the UserDB..
			JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
			File backuser     = backup.getBackUpFile(USERDB_BACKUP);
			backup.writeObjectToFile(backuser, userdb);
			
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
				BackupManager.writeObjectToFile(backuser, userdb);
				details.put("userdb", backuser.getAbsolutePath());
				
				//Now the complete SyncPackage..
				SyncPackage sp = getMainDB().getSyncPackage();
				File backsync  = backup.getBackUpFile(SYNC_BACKUP);
				BackupManager.writeObjectToFile(backsync, sp);
				details.put("chaindb", backsync.getAbsolutePath());
				
			}catch(Exception exc) {
				MinimaLogger.log("BACKUP ERROR : ");
				exc.printStackTrace();
			}
			
			//Do we shut down..
			if(shutdown) {
				Message fullshut = new Message(Main.SYSTEM_FULLSHUTDOWN);
				InputHandler.addResponseMesage(fullshut, zMessage);
				getConsensusHandler().getMainHandler().PostMessage(fullshut);
				MinimaLogger.log("Backup on shutdown fininshed..");
			}else {
				//respond..
				InputHandler.endResponse(zMessage, true, "Full Backup Performed");	
			}
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORE)) {
			//Get this as will need it a few times..
			BackupManager backup = getBackup();
			
			//Check the backups exist..
			File backuser  = backup.getBackUpFile(USERDB_BACKUP);
			File backsync  = backup.getBackUpFile(SYNC_BACKUP);
			
			//Are we ok ?
			if(!backuser.exists()) {
				//Not OK.. start fresh.. 
				MinimaLogger.log("No User backups found.. @ "+backuser.getAbsolutePath());
				getConsensusHandler().getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
			
			if(!backsync.exists()) {
				//Not OK.. start fresh.. 
				MinimaLogger.log("No SyncPackage found.. @ "+backsync.getAbsolutePath());
				getConsensusHandler().getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
			
			//Load the user..
			JavaUserDB jdb = new JavaUserDB();
			try {
				//Load the file into memory first - FAST
				byte[] userdb = BackupManager.readCompleteFile(backuser);
				ByteArrayInputStream bais = new ByteArrayInputStream(userdb);
				DataInputStream dis = new DataInputStream(bais);
				
				jdb.readDataStream(dis);
				dis.close();
				bais.close();
			}catch (Exception exc) {
				exc.printStackTrace();
				//HMM.. not good.. file corrupted.. bug out
				MinimaLogger.log("USER BACKUP FILE CORRUPTED.. not starting up.. :(");
				return;
			}
			
			//Set it..
			getMainDB().setUserDB(jdb);
			
			//Load the SyncPackage
			SyncPackage sp = new SyncPackage();
			try {
				byte[] chaindb = BackupManager.readCompleteFile(backsync);
				ByteArrayInputStream bais = new ByteArrayInputStream(chaindb);
				DataInputStream dis = new DataInputStream(bais);
				
				sp.readDataStream(dis);
				dis.close();
				bais.close();
			}catch(Exception exc) {
				exc.printStackTrace();
				//HMM.. not good.. file corrupted.. bug out
				MinimaLogger.log("SYNCPACKAGE MMR BACKUP FILE CORRUPTED.. not starting up.. :(");
				return;
			}
			
			//Get the SyncPackage
			MiniNumber casc = sp.getCascadeNode();
			
			//Drill down
			ArrayList<SyncPacket> packets = sp.getAllNodes();
			for(SyncPacket spack : packets) {
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
				ArrayList<MiniData> txns = txpow.getBlockTransactions();
				for(MiniData txn : txns) {
					TxPoW txinblock = loadTxPOW(backup.getTxpowFile(txn));
					
					//Add it..
					if(txinblock != null) {
						getMainDB().addNewTxPow(txinblock);	
					}
				}
			
				//Is this the cascade block
				if(txpow.getBlockNumber().isEqual(sp.getCascadeNode())) {
					getMainDB().hardSetCascadeNode(node);
				}
				
				//Store it..
				getBackup().backupTxpow(txpow);
			}
			
			//Reset weights
			getMainDB().hardResetChain();
			
			//And Now sort the TXPOWDB
			ArrayList<BlockTreeNode> list = getMainDB().getMainTree().getAsList();
			getMainDB().getTxPowDB().resetAllInBlocks();
			
			//Now sort
			for(BlockTreeNode treenode : list) {
				//Get the Block
				TxPoW txpow = treenode.getTxPow();
				
				//What Block
				MiniNumber block = txpow.getBlockNumber();
				
				//Set the main chain details..
				TxPOWDBRow blockrow = getMainDB().getTxPowDB().findTxPOWDBRow(txpow.getTxPowID());
				blockrow.setInBlockNumber(block);
				blockrow.setOnChainBlock(true);
				blockrow.setIsInBlock(true);
				
				//Now the Txns..
				ArrayList<MiniData> txpowlist = txpow.getBlockTransactions();
				for(MiniData txid : txpowlist) {
					TxPOWDBRow trow = getMainDB().getTxPowDB().findTxPOWDBRow(txid);
					if(trow!=null) {
						//Set that it is in this block
						trow.setOnChainBlock(false);
						trow.setIsInBlock(true);
						trow.setInBlockNumber(block);
					}
				}
			}
			
			//Get on with it..
			getConsensusHandler().getMainHandler().PostMessage(Main.SYSTEM_INIT);
		}
	}
	
	public static TxPoW loadTxPOW(File zTxpowFile) {
		if(!zTxpowFile.exists()) {
			MinimaLogger.log("Load TxPOW Doesn't exist! "+zTxpowFile.getName());
			return null;
		}
		
		//The TxPOW File..
		TxPoW txpow    = new TxPoW();
		
		try {
			//Load the complete file first..
			byte[] txfile = BackupManager.readCompleteFile(zTxpowFile);
			
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
