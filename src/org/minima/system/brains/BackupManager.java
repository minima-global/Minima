package org.minima.system.brains;

import java.io.File;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.MiniFile;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;

public class BackupManager extends SystemHandler {

	private static final String BACKUP_WRITE              = "BACKUP_WRITE";
	private static final String BACKUP_DELETE             = "BACKUP_DELETE";
	
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
	
	File mMiniDAPPS;
	
	File mWebRoot;
	
	static File mTempFolder = new File(System.getProperty("java.io.tmpdir"));
	
	public BackupManager(Main zMain, String zConfFolder) {
		super(zMain, "BACKUP");
		
		mConfigurationFolder = zConfFolder;
	
		//Start init
		initFolders();
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
	
	public File getMiniDAPPFilesFolder(String zMiniDAPPID) {
		File ff = new File(getMiniDAPPFolder(zMiniDAPPID),"files");
		ff.mkdirs();
		return ff; 
	}
	
	public File getMiniDAPPSQLFolder(String zMiniDAPPID) {
		File ff = new File(getMiniDAPPFolder(zMiniDAPPID),"sql");
		ff.mkdirs();
		return ff; 
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
			
			//Write..
			MiniFile.writeObjectToFile(ff, stream);	
		
		}else if(zMessage.isMessageType(BACKUP_DELETE)) {
			//Get the file
			File ff = (File) zMessage.getObject("file");
			MiniFile.deleteFileOrFolder(mRootPath, ff);
		}
	}
	
	private void initFolders() {
		//The Root
		mRoot      = ensureFolder(new File(mConfigurationFolder));
		mRootPath  = mRoot.getAbsolutePath();
		
		//Current used TxPOW
		mTxPOWDB   = ensureFolder(new File(mRoot,"txpow"));
		
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
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"backup"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"webroot"));
		MiniFile.deleteFileOrFolder(mRootPath,new File(zFolder,"temp"));
		
		//Don't delete the Test Web Folder..
		//?
	}
}
