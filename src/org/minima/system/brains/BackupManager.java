package org.minima.system.brains;

import java.io.File;
import java.util.Arrays;
import java.util.Comparator;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.SyncPacket;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class BackupManager extends MessageProcessor {

	private static final String BACKUP_WRITE              = "BACKUP_WRITE";
	private static final String BACKUP_DELETE             = "BACKUP_DELETE";
	
	private static final String BACKUP_CLEAN_BLOCKS       = "BACKUP_CLEAN_BLOCKS";
	private static final String BACKUP_WRITE_BLOCK        = "BACKUP_WRITE_BLOCK";
	
	//Clean up blocks every 10 minutes..
	private long CLEAN_UP_TIMER 						  = 1000 * 60 *10;
	
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
	
	File mMaximaRoot;
	
	File mTunnelRoot;
	
	static File mTempFolder = new File(System.getProperty("java.io.tmpdir"));
	
	MiniNumber mLastBlock  = MiniNumber.ZERO;
	MiniNumber mFirstBlock = MiniNumber.MINUSONE;
	
	//500,000 blocks @ 4320 blocks a day.. ~3 months
	private static MiniNumber MAX_BLOCKS  = MiniNumber.MILLION.div(MiniNumber.TWO);
	
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
	
	public File getMaximaFolder() {
		return mMaximaRoot;
	}

	public File getSSHTunnelFolder() {
		return mTunnelRoot;
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
		//Do in separate thread so returns fast
		Message backup = new Message(BackupManager.BACKUP_WRITE_BLOCK);
		backup.addObject("block", zBlock);
		PostMessage(backup);
	}

	public MiniNumber getOldestBackupBlock() {
		return mFirstBlock;
	}
	
	public void deleteTxpow(TxPoW zTxPOW) {
		//Create the File
		File delfile = new File(mTxPOWDB,zTxPOW.getTxPowID().toString()+".txpow");
		
		//Do in separate thread so returns fast
		Message delete = new Message(BackupManager.BACKUP_DELETE);
		delete.addObject("file", delfile);
		PostMessage(delete);
	}
	
	private static File ensureFolder(File zFolder) {
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
		
		}else if(zMessage.isMessageType(BACKUP_WRITE_BLOCK)) {
			//Get the Block..
			SyncPacket block = (SyncPacket) zMessage.getObject("block");
			
			//Which Block is this..
			mLastBlock = block.getTxPOW().getBlockNumber();
					
			//Get the File..v
			File savefile = getBlockFile(mLastBlock);
			//MinimaLogger.log("************ save block : "+savefile);
			
			//Write..
			MiniFile.writeObjectToFile(savefile, block);	
			
		}else if(zMessage.isMessageType(BACKUP_CLEAN_BLOCKS)) {
			//Check again
			PostTimerMessage(new TimerMessage(CLEAN_UP_TIMER, BACKUP_CLEAN_BLOCKS));
			
			//Is this the first time this has been called..
			boolean found = false;
			
			//First scan the main blocks folder and start parsing..
			File[] level1 = mBlocksDB.listFiles();
			if(level1 == null) {level1 = new File[0];}
			
			//Find the lowest block we have..
			for(File lv1 : level1) {
				//If found jump out
				if(found) {break;}
				
				//Scan lower levels
				File[] level2 = lv1.listFiles();
				if(level2 == null) {level2 = new File[0];}
				
				if(level2.length > 0) {
					for(File lv2 : level2) {
						//If found jump out
						if(found) {break;}
							
						//Check it..
						File[] files = lv2.listFiles();
						if(files == null) {files = new File[0];}
						
						//Any Files..
						if(files.length>0) {
							//Sort alphabetically..
							Arrays.sort(files, new Comparator<File>() {
								@Override
								public int compare(File arg0, File arg1) {
									return arg0.getName().compareTo(arg1.getName());
								}
							});
							
							//Get the top
							File first  = files[0];
							String name = first.getName();
							int index   = name.indexOf(".");
							name        = name.substring(0,index);
							
							mFirstBlock = new MiniNumber(name);
							found = true;
						}else {
							//MinimaLogger.log("DELETE EMPTY FOLDER "+lv2);
							MiniFile.deleteFileOrFolder(mRootPath, lv2);
						}
					}
					
				}else {
					//MinimaLogger.log("DELETE EMPTY FOLDER "+lv1);
					MiniFile.deleteFileOrFolder(mRootPath, lv1);
				}
			}
			
			//Check the scan worked
			if(!mFirstBlock.isMoreEqual(MiniNumber.ZERO) || !mLastBlock.isMore(MiniNumber.ZERO)) {
				return;
			}
			
			//Only keep MAX blocks
			MiniNumber total = mLastBlock.sub(mFirstBlock);
			if(total.isLessEqual(MAX_BLOCKS)) {
				return;
			}
			
			//How many to delete
			int delete = total.sub(MAX_BLOCKS).getAsInt(); 
			for(int i=0;i<delete;i++) {
				//Get the file..
				File ff = getBlockFile(mFirstBlock);
				
				//Delete this file..
				//MinimaLogger.log("Delete : "+ff.getAbsolutePath());
				MiniFile.deleteFileOrFolder(mRootPath, ff);
				
				//Increment the delblock
				mFirstBlock = mFirstBlock.increment();
			}
		}
	}
	
	/**
	 * The folder to store the block.. 
	 * There are 1000 blocks per folder and 1000 folders per top level folder
	 * The actual block names are zero padded.. 
	 * So that they order correctly alpha-numerically
	 * 
	 * @param zBlockNumber
	 * @return the File
	 */
	public File getBlockFile(MiniNumber zBlockNumber) {
		//Top level Folder
		MiniNumber fold1 = zBlockNumber.div(MiniNumber.MILLION).floor();
		
		//Inside Top Level
		MiniNumber remainder = zBlockNumber.sub(MiniNumber.MILLION.mult(fold1));
		MiniNumber fold2     = remainder.div(MiniNumber.THOUSAND).floor();
		
		//Get the number..
		String f1 = MiniFormat.zeroPad(6, fold1);
		String f2 = MiniFormat.zeroPad(6, fold2);
		String filename = MiniFormat.zeroPad(12, zBlockNumber);
		
		//Create the File
		File back1 = new File(mBlocksDB,f1);
		File back2 = new File(back1,f2);
		ensureFolder(back2);
		
		File savefile = new File(back2,filename+".block");
		
		return savefile;
	}
	
	private void initFolders() {
		//The Root
		mRoot      = ensureFolder(new File(mConfigurationFolder));
		mRootPath  = mRoot.getAbsolutePath();
		
		//Current used TxPOW
		mTxPOWDB   = ensureFolder(new File(mRoot,"txpow"));
		
		//Current Blocks
		mBlocksDB    = ensureFolder(new File(mRoot,"blocks"));
				
		//The Backup folder
		mBackup    = ensureFolder(new File(mRoot,"backup"));
		
		//Maxima folder
		mMaximaRoot = ensureFolder(new File(mRoot,"maxima"));
		
		//SSHTunnel folder
		mTunnelRoot = ensureFolder(new File(mRoot,"tunnel"));
		
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
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"maxima"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"tunnel"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"temp"));
	}
	
	public static void deleteWebRoot(File zFolder) {
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"webroot"));
	}
}
