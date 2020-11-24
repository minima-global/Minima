package org.minima.system.brains;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.SyncPacket;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class BackupManager extends MessageProcessor {

	private static final String BACKUP_WRITE              = "BACKUP_WRITE";
	private static final String BACKUP_DELETE             = "BACKUP_DELETE";
	private static final String BACKUP_CLEAN_BLOCKS       = "BACKUP_CLEAN_BLOCKS";
	
	private long CLEAN_UP_TIMER 						  = 10000;
	
	/**
	 * User Configuration
	 */
	String mConfigurationFolder = "";
	
	/**
	 * The Root directory..
	 */
	static File   mRoot;
	static String mRootPath = "";
	
	File mBackup;
	
	File mTxPOWDB;
	
	File mBlocksDB;
	
	File mMiniDAPPS;
	
	File mWebRoot;
	
	static File mTempFolder = new File(System.getProperty("java.io.tmpdir"));
	
	public BackupManager(String zConfFolder) {
		super("BACKUP");
		
		mConfigurationFolder = zConfFolder;
	
		//Start init
		initFolders();
		
		//A timerMessage that leans out the blocks folder..
		PostTimerMessage(new TimerMessage(CLEAN_UP_TIMER, BACKUP_CLEAN_BLOCKS));
	}
	
	public File getRootFolder() {
		return mRoot;
	}
	
	public File getTxPOWFolder() {
		return mTxPOWDB;
	}
	
	public File getMiniDAPPFolder() {
		return mMiniDAPPS;
	}
	
	public File getMiniDAPPFolder(String zMiniDAPPID) {
		return new File(getMiniDAPPFolder(),zMiniDAPPID);
	}
	
	public File getWebRoot() {
		return mWebRoot;
	}
	
	public File getBackUpFolder() {
		return mBackup;
	}
	
	public File getBackUpFile(String name) {
		return new File(mBackup, name);
	}
	
	public File getTxpowFile(MiniData zTxPoWID) {
		return new File(mTxPOWDB,zTxPoWID.to0xString()+".txpow");
	}
	
	public static File getTempFolder() {
		return mTempFolder;
	}
		
	public void backupTxpow(TxPoW zTxPOW) {
		//Create the File
		File back = new File(mTxPOWDB,zTxPOW.getTxPowID().to0xString()+".txpow");
		
		//Do in separate thread so returns fast
		Message backup = new Message(BackupManager.BACKUP_WRITE);
		backup.addObject("object", zTxPOW);
		backup.addObject("file", back);
		PostMessage(backup);
	}
	
	public void backupBlock(SyncPacket zBlock) {
		//Get the number..
		String filename = MiniFormat.zeroPad(12, zBlock.getTxPOW().getBlockNumber());
		
		//Create the File
		File back = new File(mBlocksDB,filename+".block");
		
		MinimaLogger.log("save block : "+back);
		
		//Do in separate thread so returns fast
		Message backup = new Message(BackupManager.BACKUP_WRITE);
		backup.addObject("object", zBlock);
		backup.addObject("file", back);
		PostMessage(backup);
	}

	public void deleteTxpow(TxPoW zTxPOW) {
		//Create the File
		File delfile = new File(mTxPOWDB,zTxPOW.getTxPowID().toString()+".txpow");
		
		//Do in separate thread so returns fast
		Message delete = new Message(BackupManager.BACKUP_DELETE);
		delete.addObject("file", delfile);
		PostMessage(delete);
	}
	
	private File ensureFolder(File zFolder) {
		if(!zFolder.exists()) {
			zFolder.mkdirs();
		}
		
		return zFolder;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(BACKUP_WRITE)) {
			//Get the file..
			Streamable stream = (Streamable) zMessage.getObject("object");
			
			//Get the file
			File ff = (File) zMessage.getObject("file");
			if(ff.exists()) {
				return;
			}
			
			//Write..
			MiniFile.writeObjectToFile(ff, stream);	
		
		}else if(zMessage.isMessageType(BACKUP_DELETE)) {
			//Get the file
			File ff = (File) zMessage.getObject("file");
			MiniFile.deleteFileOrFolder(mRootPath, ff);
		
		}else if(zMessage.isMessageType(BACKUP_CLEAN_BLOCKS)) {
			//Check the blocks folder and remove OLD blocks..
			File[] files = mBlocksDB.listFiles();
			
			int total = 0;
			if(files != null) {
				total = files.length;
			}
			
			MinimaLogger.log("Clean up "+total);
			
			if(total > 10) {
				//Sort alphabetically..
				Arrays.sort(files, new Comparator<File>() {
					@Override
					public int compare(File arg0, File arg1) {
						return arg0.getName().compareTo(arg1.getName());
					}
				});
				
				//Now delete the old ones..
				int delete = total - 10;
				for(int i=0;i<delete;i++) {
					//Delete these files..
					MinimaLogger.log("Delete "+files[i]);
					MiniFile.deleteFileOrFolder(mRootPath, files[i]);
				}
			}
			
			
			//Check again
			PostTimerMessage(new TimerMessage(CLEAN_UP_TIMER, BACKUP_CLEAN_BLOCKS));
		}
	}
	
	private void initFolders() {
		//The Root
		mRoot      = ensureFolder(new File(mConfigurationFolder));
		mRootPath  = mRoot.getAbsolutePath();
		
		//Current used TxPOW
		mTxPOWDB   = ensureFolder(new File(mRoot,"txpow"));
		
		//Current Blocks
		mBlocksDB   = ensureFolder(new File(mRoot,"blocks"));
				
		//The Backup folder
		mBackup    = ensureFolder(new File(mRoot,"backup"));
		
		//The Test Web folder
		mWebRoot = ensureFolder(new File(mRoot,"webroot"));
				
		//The MiniDAPPS folder
		mMiniDAPPS = ensureFolder(new File(mWebRoot,"minidapps"));
				
		//Clear temp folder..
		MiniFile.deleteFileOrFolder(mRootPath,new File(mRoot,"temp"));
		
		//Make it..
		mTempFolder = ensureFolder(new File(mRoot,"temp"));
	}
	
	public static void safeDelete(File zFile) {
		MiniFile.deleteFileOrFolder(mRootPath, zFile);
	}
	
	public static void deleteConfFolder(File zFolder) {
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"txpow"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"blocks"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"backup"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"temp"));
	}
	
	public static void deleteWebRoot(File zFolder) {
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"webroot"));
	}
}
