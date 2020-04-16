package org.minima.system.brains;

import java.io.File;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.JavaUserDB;
import org.minima.objects.Coin;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class ConsensusBackup {

	public static final String CONSENSUS_PREFIX  = "CONSENSUSBACKUP_";
	
	public static String CONSENSUSBACKUP_BACKUP  = CONSENSUS_PREFIX+"BACKUP"; 
	
	public static String CONSENSUSBACKUP_RESTORE        = CONSENSUS_PREFIX+"RESTORE"; 
	public static String CONSENSUSBACKUP_RESTOREUSERDB  = CONSENSUS_PREFIX+"RESTOREUSERDB"; 
	public static String CONSENSUSBACKUP_RESTORETXPOW   = CONSENSUS_PREFIX+"RESTORETXPOW"; 
	public static String CONSENSUSBACKUP_RESTORETREEDB  = CONSENSUS_PREFIX+"RESTORETREEDB"; 
	
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
			
			//First backup the UserDB..
			JavaUserDB userdb = (JavaUserDB) getMainDB().getUserDB();
			String nameu      = "user.minima";
			File ffu          = mHandler.getMainHandler().getBackupManager().getBackUpFile(nameu);
			Message backupu   = new Message(BackupManager.BACKUP_WRITE);
			backupu.addObject("file", ffu);
			backupu.addObject("object", userdb);
			backupu.addBoolean("overwrite", true);
			//No Post action.. we do that later..
			getBackup().PostMessage(backupu);
			
			//Get the complete List
			ArrayList<BlockTreeNode> nodes = getMainDB().getMainTree().getAsList();
			
			if(nodes.size()>0) {
				//Create a SyncPackage
				SyncPackage sp = new SyncPackage();
				
				//Cascade Node
				sp.setCascadeNode(getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber());
				
				//Cycle through it all..
				for(BlockTreeNode node : nodes) {
					sp.getAllNodes().add(0,new SyncPacket(node));
				}
				
				//The backup file
				String name = "latest.txbackup";
				File ff     = mHandler.getMainHandler().getBackupManager().getBackUpFile(name);
				
				//And the Post Message..
				Message post = new Message(Main.SYSTEM_FULLSHUTDOWN);
				
				//Send it to the Backup manager, so a separate thread is used to readwrite to the file system
				Message backup = new Message(BackupManager.BACKUP_WRITE);
				backup.addObject("file", ff);
				backup.addObject("object", sp);
				backup.addBoolean("overwrite", true);
				
				//What to do after
				if(shutdown) {
					backup.addObject(BackupManager.BACKUP_POSTACTIONMSG, post);
					backup.addObject(BackupManager.BACKUP_POSTACTION_HANDLER, mHandler.getMainHandler());
				}
				
				//And Post it
				getBackup().PostMessage(backup);
				
			}else {
				//Do we shut down..
				if(shutdown) {
					mHandler.getMainHandler().PostMessage(Main.SYSTEM_FULLSHUTDOWN);
				}
			}
			
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORE)) {
			
			//The TXPOW/TREE backup file
			String name = "user.minima";
			File ff     = getBackup().getBackUpFile(name);
			Message backupman = new Message(BackupManager.BACKUP_READ);
			
			if(ff.exists()) {
				backupman = new Message(BackupManager.BACKUP_READ);
				backupman.addObject("file", ff);
				backupman.addString("type", BackupManager.BACKUP_READUSER);
				backupman.addObject(BackupManager.BACKUP_POSTACTIONMSG, new Message(ConsensusBackup.CONSENSUSBACKUP_RESTOREUSERDB));
				backupman.addObject(BackupManager.BACKUP_POSTACTION_HANDLER,mHandler);
				getBackup().PostMessage(backupman);
			}else {
				//Get on with it..
				MinimaLogger.log("No user restore file found "+ff.getAbsolutePath()+". Start normal.");
				mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
			
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
			
			//The TXPOW/TREE backup file
			name      = "latest.txbackup";
			ff        = getBackup().getBackUpFile(name);
			
			if(ff.exists()) {
				backupman = new Message(BackupManager.BACKUP_READ);
				backupman.addObject("file", ff);
				backupman.addString("type", BackupManager.BACKUP_READSYNC);
				backupman.addObject(BackupManager.BACKUP_POSTACTIONMSG, new Message(ConsensusBackup.CONSENSUSBACKUP_RESTORETREEDB));
				backupman.addObject(BackupManager.BACKUP_POSTACTION_HANDLER,mHandler);
				getBackup().PostMessage(backupman);
			}else {
				//Get on with it..
				MinimaLogger.log("No tree backup file found "+ff.getAbsolutePath()+". Start normal.");
				mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
				return;
			}
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTOREUSERDB)) {
			//Get the DB..
			JavaUserDB jdb = (JavaUserDB) zMessage.getObject("readobject");
			
			//Set it..
			getMainDB().setUserDB(jdb);
		
		}else if(zMessage.isMessageType(CONSENSUSBACKUP_RESTORETXPOW)) {
			TxPOW txPOW = (TxPOW) zMessage.getObject("readobject");
			
			//Add to the database..
			getMainDB().addNewTxPow(txPOW);
			
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
			
			//Get on with it..
			mHandler.getMainHandler().PostMessage(Main.SYSTEM_INIT);
		}
	}
}
