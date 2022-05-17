package org.minima.database;

import java.io.File;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.minidapps.MiniDAPPDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.userprefs.UserDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.wallet.Wallet;
import org.minima.system.network.p2p.P2PDB;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

public class MinimaDB {

	/**
	 * Static access to MAIN MinimaDB
	 */
	private static MinimaDB mMinimaDB = null;
	public static void createDB() {
		mMinimaDB = new MinimaDB();
	}
	public static MinimaDB getDB() {return mMinimaDB;}
	
	/**
	 * The individual DBs
	 */
	ArchiveManager 	mArchive;
	TxPoWDB 		mTxPoWDB;
	TxPowTree 		mTxPoWTree;
	Cascade			mCacscade;
	UserDB			mUserDB;
	TxnDB			mTxnDB;
	Wallet			mWallet;
	
	/**
	 * Temporary
	 */
	MiniDAPPDB		mMiniDAPP;
	
	/**
	 * For P2P Information
	 */
	P2PDB			mP2PDB;
	
	/**
	 * LOCKING the MinimaDB for read write operations..
	 */
	ReadWriteLock mRWLock;
	
	/**
	 * Main Constructor
	 */
	public MinimaDB() {
		mArchive	= new ArchiveManager();
		mTxPoWDB	= new TxPoWDB();
		mTxPoWTree 	= new TxPowTree();
		mCacscade	= new Cascade();
		mUserDB		= new UserDB();
		mWallet		= new Wallet();
		
		mMiniDAPP   = new MiniDAPPDB();
		
		mP2PDB		= new P2PDB();
		
		mRWLock = new ReentrantReadWriteLock();
	}
	
	/**
	 * Locking access to the database is CRUCIAL
	 * 
	 * You need to do this for the IBD and Greeting messages so they are consistent
	 */
	public void readLock(boolean zLock) {
		if(zLock) {
			mRWLock.readLock().lock();
		}else {
			mRWLock.readLock().unlock();
		}
	}
	
	public void writeLock(boolean zLock) {
		if(zLock) {
			mRWLock.writeLock().lock();
		}else {
			mRWLock.writeLock().unlock();
		}
	}
	
	/**
	 * Get the various different databases
	 */
	public TxPoWDB getTxPoWDB() {
		return mTxPoWDB;
	}
	
	public TxPowTree getTxPoWTree() {
		return mTxPoWTree;
	}
	
	public Cascade getCascade() {
		return mCacscade;
	}
	
	public void setIBDCascade(Cascade zCascade) {
		mCacscade = zCascade;
	}
	
	public long getCascadeFileSize() {
		return getDBFileSie("cascade.db");
	}
	
	public long getUserDBFileSize() {
		return getDBFileSie("userprefs.db");
	}
	
	public long getTxPowTreeFileSize() {
		return getDBFileSie("chaintree.db");
	}
	
	public long getP2PFileSize() {
		return getDBFileSie("p2p.db");
	}
	
	public MiniDAPPDB getMiniDAPPDB() {
		return mMiniDAPP;
	}
	
	private long getDBFileSie(String zFilename) {
		//Get the base Database folder
		File basedb = getBaseDBFolder();
		
		//The File
		File file = new File(basedb,zFilename);
		if(file.exists()) {
			return file.length();
		}
		
		return 0;
	}
	
	public UserDB getUserDB() {
		return mUserDB;
	}
	
	public TxnDB getCustomTxnDB() {
		return mTxnDB;
	}
	
	public Wallet getWallet() {
		return mWallet;
	}
	
	public ArchiveManager getArchive() {
		return mArchive;
	}
	
	public P2PDB getP2PDB() {
		return mP2PDB;
	}
	
	public File getBaseDBFolder() {
		return new File(GeneralParams.DATA_FOLDER,"databases");
	}
	
	public void loadAllDB() {
		
		//We need read lock 
		writeLock(true);
		
		try {
			
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//Set the Archive folder
			File archsqlfolder = new File(basedb,"archivesql");
			mArchive.loadDB(new File(archsqlfolder,"archive"));
			
			//Load the wallet
			File walletsqlfolder = new File(basedb,"walletsql");
			mWallet.loadDB(new File(walletsqlfolder,"wallet"));
			//mWallet.initDefaultKeys();
			
			//Load the SQL DB
			File txpowsqlfolder = new File(basedb,"txpowsql");
			mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
			
			//Load the User Prefs
			mUserDB.loadDB(new File(basedb,"userprefs.db"));
			
			//Load the custom Txns..
			mTxnDB = new TxnDB();
			mTxnDB.loadDB();
			
			//Load the Cascade
			mCacscade.loadDB(new File(basedb,"cascade.db"));
			
			//Load the TxPoWTree
			mTxPoWTree.loadDB(new File(basedb,"chaintree.db"));
			
			//And finally..
			mP2PDB.loadDB(new File(basedb,"p2p.db"));
			
			//Temp MiniDAPP DB
			mMiniDAPP.loadDB(new File(basedb,"minidapp.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR loadAllDB "+exc);
		}
		
		//Release the krakken
		writeLock(false);
	}
	
	public void saveAllDB() {
		//First the SQL
		saveSQL();
		
		//And the rest
		saveState();
	}
	
	public void saveSQL() {
		//We need read lock 
		readLock(true);
		
		try {
			//Clean shutdown of SQL DBs
			mTxPoWDB.saveDB();
			mArchive.saveDB();
			mWallet.saveDB();
			
			//Temp
			mMiniDAPP.saveDB();
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR saveSQL "+exc);
		}
		
		//Release the krakken
		readLock(false);
	}
	
	public void saveWalletSQL() {
		//We need read lock 
		readLock(true);
		
		try {
			mWallet.saveDB();
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR saveWalletSQL "+exc);
		}
		
		//Release the krakken
		readLock(false);
	}
	
	public void saveState() {
		
		//We need read lock 
		readLock(true);
		
		try {
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//JsonDBs
			mTxnDB.saveDB();
			mUserDB.saveDB(new File(basedb,"userprefs.db"));
			mP2PDB.saveDB(new File(basedb,"p2p.db"));
			
			//Custom
			mCacscade.saveDB(new File(basedb,"cascade.db"));
			mTxPoWTree.saveDB(new File(basedb,"chaintree.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR saving state "+exc);
		}
		
		//Release the krakken
		readLock(false);
	}
	
	public void saveUserDB() {
		
		//Get the base Database folder
		File basedb = getBaseDBFolder();
		
		//JsonDBs
		mUserDB.saveDB(new File(basedb,"userprefs.db"));
		
	}
}
