package org.minima.system.brains;

import java.io.File;
import java.util.Arrays;
import java.util.Comparator;

import org.minima.database.txpowtree.BlockTreeNode;
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
//	private static final String BACKUP_WRITE_BLOCK        = "BACKUP_WRITE_BLOCK";

	private static final String BACKUP_WRITE_TEMPBLOCKID  = "BACKUP_WRITE_TEMPBLOCKID";
	private static final String BACKUP_SAVE_TEMPBLOCKID   = "BACKUP_SAVE_TEMPBLOCKID";
	
	//Clean up blocks every 10 minutes..
	private long CLEAN_UP_TIMER = 1000 * 60 * 10;
	
	//For Now.. 1 month
	private static MiniNumber MAX_BLOCKS  = new MiniNumber(4320 * 30);
		
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
	
	File mTempBlocksDB;
	
	File mBlocksDB;
	
	File mMiniDAPPS;
	
	File mWebRoot;
	
	File mMaximaRoot;
	
	File mTunnelRoot;
	
	static File mTempFolder = new File(System.getProperty("java.io.tmpdir"));
	
	MiniNumber mLastBlock  = MiniNumber.ZERO;
	MiniNumber mFirstBlock = MiniNumber.MINUSONE;
	
	
	public BackupManager(String zConfFolder) {
		super("BACKUP");
		
		mConfigurationFolder = zConfFolder;
	
		//Start init
		initFolders();
		
		//A timerMessage that leans out the blocks folder..
		PostMessage(BACKUP_CLEAN_BLOCKS);
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
	
	public File getTempBlockDB() {
		return mTempBlocksDB;
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
	
//	public void backupBlock(SyncPacket zBlock) {
//		//Do in separate thread so returns fast
//		Message backup = new Message(BackupManager.BACKUP_WRITE_BLOCK);
//		backup.addObject("block", zBlock);
//		PostMessage(backup);
//	}
	
//	public void backupTempBlock(SyncPacket zBlock) {
//		//Do in separate thread so returns fast
//		Message backup = new Message(BackupManager.BACKUP_WRITE_TEMPBLOCKID);
//		backup.addObject("block", zBlock);
//		PostMessage(backup);
//	}
	
	public void backupTempBlock(BlockTreeNode zBlock) {
		//Backup the Temp block
		TxPoW copytx = zBlock.getTxPow().deepCopy();
		copytx.clearBody();
		
		//Now make a tree node..
		BlockTreeNode copynode = new BlockTreeNode(copytx);
		copynode.setMMRset(zBlock.getMMRSet().deepCopy());
		copynode.setCascade(false);
		
		//Now make a syncpacket
		SyncPacket pack = new SyncPacket(copynode, false);
		
		//Do in separate thread so returns fast
		Message backup = new Message(BackupManager.BACKUP_WRITE_TEMPBLOCKID);
		backup.addObject("block", pack);
		PostMessage(backup);
	}
	
	public void backupSaveTempBlock(MiniNumber zBlockNumber, String zTxPoWID) {
		//Do in separate thread so returns fast
		Message backup = new Message(BackupManager.BACKUP_SAVE_TEMPBLOCKID);
		backup.addObject("block", zBlockNumber);
		backup.addObject("txpowid", zTxPoWID);
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
		
		//And Delete the possible Block ID file..
		File delblock = new File(mTempBlocksDB,zTxPOW.getTxPowID().toString()+".block");
		delete = new Message(BackupManager.BACKUP_DELETE);
		delete.addObject("file", delblock);
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
		
//		}else if(zMessage.isMessageType(BACKUP_WRITE_BLOCK)) {
//			//Get the Block..
//			SyncPacket block = (SyncPacket) zMessage.getObject("block");
//			
//			//Which Block is this..
//			mLastBlock = block.getTxPOW().getBlockNumber();
//					
//			//Get the File..v
//			File savefile = getBlockFile(mLastBlock);
//			//MinimaLogger.log("************ save block : "+savefile);
//			
//			//Write..
//			MiniFile.writeObjectToFile(savefile, block);	
		
		}else if(zMessage.isMessageType(BACKUP_WRITE_TEMPBLOCKID)) {
			//Get the Block..
			SyncPacket block = (SyncPacket) zMessage.getObject("block");
			
			//Write it out with the TxPowID as the filename..
			String ID = block.getTxPOW().getTxPowID().to0xString();
					
			//Get the File..v
			File savefile = new File(mTempBlocksDB, ID+".block");
			
//			MinimaLogger.log("************ save ID block : "+savefile);
			
			//Write..
			MiniFile.writeObjectToFile(savefile, block);	
		
		}else if(zMessage.isMessageType(BACKUP_SAVE_TEMPBLOCKID)) {
			//Get the Block ID..
			MiniNumber block = (MiniNumber) zMessage.getObject("block");
			String txpowid   = zMessage.getString("txpowid");
			
			//Get the File..
			File blockfile = new File(mTempBlocksDB, txpowid+".block");
			
			//Which Block is this..
			if(block.isMore(mLastBlock)) {
				mLastBlock = block;
			}
			
//			MinimaLogger.log("BLOCKS "+mFirstBlock+" - "+mLastBlock);
			
			//Get the File..v
			File savefile = getBlockFile(block);
			
			//Now copy from one to the other
			MiniFile.copyFile(blockfile, savefile);
			
//			MinimaLogger.log("************ COPY ID block : "+blockfile+" to "+savefile);
			
		}else if(zMessage.isMessageType(BACKUP_CLEAN_BLOCKS)) {
			//Check again
			PostTimerMessage(new TimerMessage(CLEAN_UP_TIMER, BACKUP_CLEAN_BLOCKS));
			
			//Is this the first time this has been called..
			boolean found = false;
			
			//First scan the main blocks folder and start parsing..
			File[] level1 = mBlocksDB.listFiles();
			if(level1 == null){level1 = new File[0];}
			
			sortFilesByNumber(level1);
					
			//Find the lowest block we have..
			for(File lv1 : level1) {
				//If found jump out
				if(found) {break;}
				
				//Scan lower levels
				File[] level2 = lv1.listFiles();
				if(level2 == null) {level2 = new File[0];}
				
				sortFilesByNumber(level2);
				
				if(level2.length > 0) {
					for(File lv2 : level2) {
						//If found jump out
						if(found) {break;}
							
						//Check it..
						File[] files = lv2.listFiles();
						if(files == null) {files = new File[0];}
						
						//Any Files..
						if(files.length>0) {
							sortFilesByNumber(files);
							
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
			
//			MinimaLogger.log("FIRST BackUp Block : "+mFirstBlock);
			
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
	
	private void sortFilesByNumber(File[] zFiles) {
		//Sort alphabetically..
		Arrays.sort(zFiles, new Comparator<File>() {
			@Override
			public int compare(File arg0, File arg1) {
				return arg0.getName().compareTo(arg1.getName());
			}
		});
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
		
		//Folder with ALL the txpow blocks named after the TxPoWID
		mTempBlocksDB = ensureFolder(new File(mRoot,"blockstempid")); 
		
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
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"blockstempid"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"backup"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"maxima"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"tunnel"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"temp"));
	}
	
	public static void deleteWebRoot(File zFolder) {
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"webroot"));
	}
}
