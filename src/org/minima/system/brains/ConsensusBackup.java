package org.minima.system.brains;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.JavaUserDB;
import org.minima.objects.Coin;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;

public class ConsensusBackup {

	public static final String CONSENSUS_PREFIX  = "CONSENSUSBACKUP_";
	
	public static String CONSENSUSBACKUP_BACKUP  = CONSENSUS_PREFIX+"BACKUP"; 
	
	public static String CONSENSUSBACKUP_RESTORE        = CONSENSUS_PREFIX+"RESTORE"; 
	public static String CONSENSUSBACKUP_RESTOREUSERDB  = CONSENSUS_PREFIX+"RESTOREUSERDB"; 
	public static String CONSENSUSBACKUP_RESTORETXPOW   = CONSENSUS_PREFIX+"RESTORETXPOW"; 
	public static String CONSENSUSBACKUP_RESTORETREEDB  = CONSENSUS_PREFIX+"RESTORETREEDB"; 
	
	public static final String USERDB_BACKUP = "user.minima";
	public static final String SYNC_BACKUP   = "sync.package";
	
	
	MinimaDB mDB;
	ConsensusHandler mHandler;
	
	public ConsensusBackup(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB = zDB;
		mHandler = zHandler;
	}
	
	private MinimaDB getMainDB() {
		return mDB;
	}
	
	private BackupManager getBackup() {
		return mHandler.getMainHandler().getBackupManager();
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUSBACKUP_BACKUP)) {
			//Is this for shut down or just a regular backup..
			boolean shutdown = false;
			if(zMessage.exists("shutdown")) {
				shutdown = zMessage.getBoolean("shutdown");
			}
			
			//Get this as will need it a few times..
			BackupManager backup = mHandler.getMainHandler().getBackupManager();
			
			//First backup the UserDB..
			JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
			File backuser     = backup.getBackUpFile(USERDB_BACKUP);
			writeObjectToFile(backuser, userdb);
			
			//Now the complete SyncPackage..
			SyncPackage sp = getMainDB().getSyncPackage();
			File backsync  = backup.getBackUpFile(SYNC_BACKUP);
			writeObjectToFile(backsync, sp);
			
			//Do we shut down..
			if(shutdown) {
				mHandler.getMainHandler().PostMessage(Main.SYSTEM_FULLSHUTDOWN);
			}
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORE)) {
			
			//Get this as will need it a few times..
			BackupManager backup = mHandler.getMainHandler().getBackupManager();
			
			//Check the backups exist..
			File backuser  = backup.getBackUpFile(USERDB_BACKUP);
			File backsync  = backup.getBackUpFile(SYNC_BACKUP);
			
			//Are we ok ?
			if(!backuser.exists() || !backsync.exists()) {
				//Not OK.. start fresh.. 
				MinimaLogger.log("No backups found.. Start normal.");
				mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
			
			//Load the user..
			FileInputStream fis = new FileInputStream(backuser);
			DataInputStream dis = new DataInputStream(fis);
			JavaUserDB jdb = new JavaUserDB();
			jdb.readDataStream(dis);
			dis.close();
			fis.close();
			
			//Set it..
			getMainDB().setUserDB(jdb);
			
			//Load all the TXPOW
			File[] txpows = getBackup().getTxPOWFolder().listFiles();
			for(File txf : txpows) {
				mHandler.PostMessage(new Message(CONSENSUSBACKUP_RESTORETXPOW).addObject("file", txf));
			}
			
			//Load the SyncPackage
			fis = new FileInputStream(backsync);
			dis = new DataInputStream(fis);
			SyncPackage sp = new SyncPackage();
			sp.readDataStream(dis);
			dis.close();
			fis.close();
			
			//And send it on..
			Message syncp = new Message(CONSENSUSBACKUP_RESTORETREEDB);
			syncp.addObject("readobject", sp);
			mHandler.PostMessage(syncp);
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORETXPOW)) {
			File ff = (File) zMessage.getObject("file");
			
			//Load it..
			FileInputStream fis = new FileInputStream(ff);
			DataInputStream dis = new DataInputStream(fis);
			TxPOW txpow    = new TxPOW();
			txpow.readDataStream(dis);
			dis.close();
			fis.close();
			
			//Add it.. will sort it out in the next step..
			getMainDB().addNewTxPow(txpow);
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORETREEDB)) {
			//Get the SyncPackage
			SyncPackage sp = (SyncPackage) zMessage.getObject("readobject");
			MiniNumber casc = sp.getCascadeNode();
			
			//Clear the database..
			getMainDB().getTxPowDB().ClearDB();
			
			//Drill down 
			ArrayList<SyncPacket> packets = sp.getAllNodes();
			for(SyncPacket spack : packets) {
				TxPOW txpow = spack.getTxPOW();
				MMRSet mmrset  = spack.getMMRSet();
				boolean cascade = spack.isCascade();
				
				//Check all MMR in the unbroken chain.. no point in cascade as may have changed..
				if(mmrset!=null) {
					if(mmrset.getBlockTime().isMoreEqual(casc)) {
						getMainDB().scanMMRSetForCoins(mmrset);
					}
				}
				
				//Add it to the DB..
				BlockTreeNode node = getMainDB().hardAddTxPOWBlock(txpow, mmrset, cascade);
			
				//Is this the cascade block
				if(txpow.getBlockNumber().isEqual(sp.getCascadeNode())) {
					getMainDB().hardSetCascadeNode(node);
				}
			}
			
			//Reset weights
			getMainDB().hardResetChain();
			
			//And Now sort the TXPOWDB
			ArrayList<BlockTreeNode> list = getMainDB().getMainTree().getAsList();
			getMainDB().getTxPowDB().resetAllInBlocks();
			getMainDB().getCoinDB().clearDB();
			
			//Only add coins from the cascade onwards..
			MiniNumber oldcascade = getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber();
			
			//Reverse the list
			Collections.reverse(list);
			
			//Now sort
			for(BlockTreeNode treenode : list) {
				//Get the Block
				TxPOW txpow = treenode.getTxPow();
				
				//get the row..
				TxPOWDBRow trow = getMainDB().getTxPowDB().addTxPOWDBRow(txpow);
				
				//What Block
				MiniNumber block = txpow.getBlockNumber();
				
				//Set the details
				trow.setOnChainBlock(true);
				trow.setIsInBlock(true);
				trow.setInBlockNumber(block);
				
				if(treenode.getTxPow().getBlockNumber().isMoreEqual(oldcascade)) {
					//Check for coins in the MMR
					getMainDB().scanMMRSetForCoins(treenode.getMMRSet());
				}
				
				//Now the Txns..
				ArrayList<MiniData> txpowlist = txpow.getBlockTxns();
				for(MiniData txid : txpowlist) {
					trow = getMainDB().getTxPowDB().findTxPOWDBRow(txid);
					if(trow!=null) {
						//Set that it is in this block
						trow.setOnChainBlock(false);
						trow.setIsInBlock(true);
						trow.setInBlockNumber(block);
					}
				}
			}
			
			//Get on with it..
			mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
		}
	
//		if(zMessage.isMessageType(CONSENSUSBACKUP_BACKUP)) {
//			//Is this for shut down or just a regular backup..
//			boolean shutdown = false;
//			if(zMessage.exists("shutdown")) {
//				shutdown = zMessage.getBoolean("shutdown");
//			}
//			
//			//First backup the UserDB..
//			JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
//			String nameu      = "user.minima";
//			File ffu          = mHandler.getMainHandler().getBackupManager().getBackUpFile(nameu);
//			Message backupu   = new Message(BackupManager.BACKUP_WRITE);
//			backupu.addObject("file", ffu);
//			backupu.addObject("object", userdb);
//			backupu.addBoolean("overwrite", true);
//			//No Post action.. we do that later..
//			getBackup().PostMessage(backupu);
//			
//			//Get the complete List
//			ArrayList<BlockTreeNode> nodes = getMainDB().getMainTree().getAsList();
//			
//			if(nodes.size()>0) {
//				//Create a SyncPackage
//				SyncPackage sp = new SyncPackage();
//				
//				//Cascade Node
//				sp.setCascadeNode(getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber());
//				
//				//Cycle through it all..
//				for(BlockTreeNode node : nodes) {
//					sp.getAllNodes().add(0,new SyncPacket(node));
//				}
//				
//				//The backup file
//				String name = "latest.txbackup";
//				File ff     = mHandler.getMainHandler().getBackupManager().getBackUpFile(name);
//				
//				//And the Post Message..
//				Message post = new Message(Main.SYSTEM_FULLSHUTDOWN);
//				
//				//Send it to the Backup manager, so a separate thread is used to readwrite to the file system
//				Message backup = new Message(BackupManager.BACKUP_WRITE);
//				backup.addObject("file", ff);
//				backup.addObject("object", sp);
//				backup.addBoolean("overwrite", true);
//				
//				//What to do after
//				if(shutdown) {
//					backup.addObject(BackupManager.BACKUP_POSTACTIONMSG, post);
//					backup.addObject(BackupManager.BACKUP_POSTACTION_HANDLER, mHandler.getMainHandler());
//				}
//				
//				//And Post it
//				getBackup().PostMessage(backup);
//				
//			}else {
//				//Do we shut down..
//				if(shutdown) {
//					mHandler.getMainHandler().PostMessage(Main.SYSTEM_FULLSHUTDOWN);
//				}
//			}
//			
//		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORE)) {
//			
//			//The TXPOW/TREE backup file
//			String name = "user.minima";
//			File ff     = getBackup().getBackUpFile(name);
//			Message backupman = new Message(BackupManager.BACKUP_READ);
//			
//			if(ff.exists()) {
//				backupman = new Message(BackupManager.BACKUP_READ);
//				backupman.addObject("file", ff);
//				backupman.addString("type", BackupManager.BACKUP_READUSER);
//				backupman.addObject(BackupManager.BACKUP_POSTACTIONMSG, new Message(ConsensusBackup.CONSENSUSBACKUP_RESTOREUSERDB));
//				backupman.addObject(BackupManager.BACKUP_POSTACTION_HANDLER,mHandler);
//				getBackup().PostMessage(backupman);
//			}else {
//				//Get on with it..
//				MinimaLogger.log("No user restore file found "+ff.getAbsolutePath()+". Start normal.");
//				mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
//				return;
//			}
//			
//			//Load all the TxPOW files..
//			File[] txpows = getBackup().getTxPOWFolder().listFiles();
//			for(File txf : txpows) {
//				//Lock and load
//				backupman = new Message(BackupManager.BACKUP_READ);
//				backupman.addObject("file", txf);
//				backupman.addString("type", BackupManager.BACKUP_READTXPOW);
//				
//				backupman.addObject(BackupManager.BACKUP_POSTACTIONMSG, new Message(ConsensusBackup.CONSENSUSBACKUP_RESTORETXPOW));
//				backupman.addObject(BackupManager.BACKUP_POSTACTION_HANDLER,mHandler);
//				getBackup().PostMessage(backupman);
//			}
//			
//			//The TXPOW/TREE backup file
//			name  = "latest.txbackup";
//			ff    = getBackup().getBackUpFile(name);
//			
//			if(ff.exists()) {
//				backupman = new Message(BackupManager.BACKUP_READ);
//				backupman.addObject("file", ff);
//				backupman.addString("type", BackupManager.BACKUP_READSYNC);
//				backupman.addObject(BackupManager.BACKUP_POSTACTIONMSG, new Message(ConsensusBackup.CONSENSUSBACKUP_RESTORETREEDB));
//				backupman.addObject(BackupManager.BACKUP_POSTACTION_HANDLER,mHandler);
//				getBackup().PostMessage(backupman);
//			}else {
//				//Get on with it..
//				MinimaLogger.log("No tree backup file found "+ff.getAbsolutePath()+". Start normal.");
//				mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
//				return;
//			}
//		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTOREUSERDB)) {
//			//Get the DB..
//			JavaUserDB jdb = (JavaUserDB) zMessage.getObject("readobject");
//			
//			//Set it..
//			getMainDB().setUserDB(jdb);
//		
//		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORETXPOW)) {
//			TxPOW txPOW = (TxPOW) zMessage.getObject("readobject");
//			
//			//Add to the database..
//			System.out.println();
//			
//			getMainDB().addNewTxPow(txPOW);
//			
//		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORETREEDB)) {
//			//Get the SyncPackage
//			SyncPackage sp = (SyncPackage) zMessage.getObject("readobject");
//			MiniNumber casc = sp.getCascadeNode();
//			
//			//Clear the database..
//			getMainDB().getTxPowDB().ClearDB();
//			
//			//Drill down 
//			ArrayList<SyncPacket> packets = sp.getAllNodes();
//			for(SyncPacket spack : packets) {
//				TxPOW txpow = spack.getTxPOW();
//				MMRSet mmrset  = spack.getMMRSet();
//				boolean cascade = spack.isCascade();
//				
//				//Check all MMR in the unbroken chain.. no point in cascade as may have changed..
//				if(mmrset!=null) {
//					if(mmrset.getBlockTime().isMoreEqual(casc)) {
//						getMainDB().scanMMRSetForCoins(mmrset);
//					}
//				}
//				
//				//Add it to the DB..
//				BlockTreeNode node = getMainDB().hardAddTxPOWBlock(txpow, mmrset, cascade);
//			
//				//Is this the cascade block
//				if(txpow.getBlockNumber().isEqual(sp.getCascadeNode())) {
//					getMainDB().hardSetCascadeNode(node);
//				}
//			}
//			
//			//Reset weights
//			getMainDB().hardResetChain();
//			
//			//Get on with it..
//			mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
//		}
	}
	
	/**
	 * Store a Streamable Object to a file.
	 * 
	 * @param zFile
	 * @param zObject
	 * @throws IOException
	 */
	public static void writeObjectToFile(File zFile, Streamable zObject) throws IOException {
		//Delete the old..
		if(zFile.exists()) {
			//Should probably just move it here - as a backup incase of error..
			zFile.delete();
		}
		
		//Create the new..
		zFile.createNewFile();
		
		//Write it out..
		FileOutputStream fos = new FileOutputStream(zFile, false);
		DataOutputStream dos = new DataOutputStream(fos);
		
		//And write it..
		zObject.writeDataStream(dos);
		
		//flush
		dos.flush();
		fos.flush();
	}
}
