package org.minima.system.backup;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;

public class BackupManager extends SystemHandler {

	private static final String BACKUP_INIT               = "BACKUP_INIT";
	private static final String BACKUP_CLEAR              = "BACKUP_CLEAR";	
	private static final String BACKUP_WRITE              = "BACKUP_WRITE";
	private static final String BACKUP_DELETE             = "BACKUP_DELETE";
	
	/**
	 * User Configuration
	 */
	String mConfigurationFolder = "";
	
	/**
	 * The Root directory..
	 */
	File mRoot;
	
	File mBackup;
	
	File mTxPOWDB;
	
	File mMiniDAPPS;
	
	public BackupManager(Main zMain, String zConfFolder) {
		super(zMain, "BACKUP");
		
		mConfigurationFolder = zConfFolder;
	
		//Start init
		PostMessage(BACKUP_INIT);
	}
	
	public File getRootFolder() {
		return mRoot;
	}
	
	public File getTxPOWFolder() {
		return mTxPOWDB;
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
		
	public void backupTxpow(TxPOW zTxPOW) {
		//Create the File
		File back = new File(mTxPOWDB,zTxPOW.getTxPowID().to0xString()+".txpow");

		//does it already exist..
		if(back.exists()) {
			return;
		}
		
		//Do in separate thread so returns fast
		Message backup = new Message(BackupManager.BACKUP_WRITE);
		backup.addObject("object", zTxPOW);
		backup.addObject("file", back);
		PostMessage(backup);
	}

	public void deleteTxpow(TxPOW zTxPOW) {
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
		
		if(zMessage.isMessageType(BACKUP_INIT)) {
			initFolders();
			
		}else if(zMessage.isMessageType(BACKUP_CLEAR)) {
			File root = new File(mConfigurationFolder);
			
			if(root.exists()) {
				MinimaLogger.log("Wiping Minima Folder : "+root.getAbsolutePath());
				deleteFileOrFolder(root);
			}
			
			initFolders();
		
		}else if(zMessage.isMessageType(BACKUP_WRITE)) {
			//Get the file..
			Streamable stream = (Streamable) zMessage.getObject("object");
			
			//Get the file
			File ff = (File) zMessage.getObject("file");
			
			//Write..
			writeObjectToFile(ff, stream);	
		
		}else if(zMessage.isMessageType(BACKUP_DELETE)) {
			//Get the file
			File ff = (File) zMessage.getObject("file");
			deleteFileOrFolder(ff);
		}
	}
	
	private void initFolders() {
		//The Root
		mRoot = ensureFolder(new File(mConfigurationFolder));
		
		//Current used TxPOW
		mTxPOWDB = ensureFolder(new File(mRoot,"txpow"));
		
		//The Backup folder
		mBackup  = ensureFolder(new File(mRoot,"backup"));
		
		//The MiniDAPPS folder
		mMiniDAPPS = ensureFolder(new File(mRoot,"minidapps"));
	}
	
	public static void deleteAllButMiniDAPPS(File zFolder) {
		deleteFileOrFolder(new File(zFolder,"txpow"));
		deleteFileOrFolder(new File(zFolder,"backup"));
	}
	
	/**
	 * Delete a file or folder and it's children
	 * @param zFile
	 */
	public static void deleteFileOrFolder(File zFile) {
		//Check for real
		if(zFile == null || !zFile.exists()) {
			return;
		}
		
		//Scan if Directory
		if(zFile.isDirectory()) {
			//List the files..
			File[] files = zFile.listFiles();
			if(files != null) {
				for(File ff : files) {
					deleteFileOrFolder(ff);
				}
			}	
		}
		
		//And finally delete the actual file.. (double check is a minima file.. ROUGH check..)
		if(zFile.getAbsolutePath().toLowerCase().contains("minima")) {
			zFile.delete();
		}
	}
	
	
	/**
	 * Store a Streamable Object to a file.
	 * 
	 * @param zFile
	 * @param zObject
	 * @throws IOException
	 */
	public static void writeObjectToFile(File zFile, Streamable zObject) throws IOException {
		//Check Parent
		File parent = zFile.getParentFile();
		if(!parent.exists()) {
			parent.mkdirs();
		}
		
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
