package org.minima.system.backup;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;

public class BackupManager extends SystemHandler {

	private static final String BACKUP_INIT               = "BACKUP_INIT";
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
	static String mRootPath = "***";
	
	File mBackup;
	
	File mTxPOWDB;
	
	File mMiniDAPPS;
	
	static File mTempFolder = new File(System.getProperty("java.io.tmpdir"));
	
	public BackupManager(Main zMain, String zConfFolder) {
		super(zMain, "BACKUP");
		
		mConfigurationFolder = zConfFolder;
	
		//Start init
		initFolders();
//		PostMessage(BACKUP_INIT);
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
		
		if(zMessage.isMessageType(BACKUP_INIT)) {
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
		mRoot      = ensureFolder(new File(mConfigurationFolder));
		mRootPath  = mRoot.getAbsolutePath();
		
		//Current used TxPOW
		mTxPOWDB   = ensureFolder(new File(mRoot,"txpow"));
		
		//The Backup folder
		mBackup    = ensureFolder(new File(mRoot,"backup"));
		
		//The MiniDAPPS folder
		mMiniDAPPS = ensureFolder(new File(mRoot,"minidapps"));
		
		//TEMP folder for file upload in MiniDAPPS
		File temp = new File(mRoot,"temp");
		
		//Clear it..
		deleteFileOrFolder(temp);
		
		//Make it..
		mTempFolder = ensureFolder(new File(mRoot,"temp"));
	}
	
	public static void deleteConfFolder(File zFolder) {
		deleteFileOrFolder(new File(zFolder,"txpow"));
		deleteFileOrFolder(new File(zFolder,"backup"));
		deleteFileOrFolder(new File(zFolder,"minidapps"));
		deleteFileOrFolder(new File(zFolder,"temp"));
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
		
		//And finally delete the actual file.. (double check is a minima file.. )
		if(mRootPath.equals("***")) {
			zFile.delete();
		}else if(zFile.getAbsolutePath().startsWith(mRootPath)) {
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
		//First write the object to a memory structure..
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		
		zObject.writeDataStream(dos);
		
		dos.flush();
		baos.flush();
	
		//get all the data
		byte[] data = baos.toByteArray();
		
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
		DataOutputStream fdos = new DataOutputStream(fos);
		
		//And write it..
		fdos.write(data);
		//zObject.writeDataStream(fdos);
		
		//flush
		fdos.flush();
		fos.flush();
	}
	
	public static byte[] readCompleteFile(File zFile) throws IOException {
		//How big is this file..
		long len = zFile.length();
		
		//Create the data structure to hold it..
		byte[] data = new byte[(int)len];
		
		FileInputStream fis = new FileInputStream(zFile);
		DataInputStream dis = new DataInputStream(fis);
		dis.readFully(data);
		dis.close();
		fis.close();
		
		return data;
	}
}
