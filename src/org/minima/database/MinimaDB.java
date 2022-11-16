package org.minima.database;

import java.io.File;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.userprefs.UserDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.wallet.Wallet;
import org.minima.system.network.p2p.P2PDB;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
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
	Cascade			mCascade;
	UserDB			mUserDB;
	TxnDB			mTxnDB;
	Wallet			mWallet;
	MaximaDB	 	mMaximaDB;
	MDSDB			mMDSDB;
	
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
		mCascade	= new Cascade();
		mUserDB		= new UserDB();
		mWallet		= new Wallet();
		mMaximaDB	= new MaximaDB();
		mMDSDB   	= new MDSDB();
		
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
		return mCascade;
	}
	
	public void setIBDCascade(Cascade zCascade) {
		mCascade = zCascade;
	}
	
	//Used when doing an archive resync
	public void resetCascadeAndTxPoWTree() {
		mCascade 	= new Cascade();
		mTxPoWTree 	= new TxPowTree();
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
	
	public MDSDB getMDSDB() {
		return mMDSDB;
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
	
	public MaximaDB getMaximaDB() {
		return mMaximaDB;
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
			try {
				
				//Try and load the Archive DB
				mArchive.loadDB(new File(archsqlfolder,"archive"));
				
//				throw new Exception("TEST CRASH ARCHIVEDB!");
				
			}catch(Exception exc) {
				
				//Log the complete error
				MinimaLogger.log(exc);
				
				//There wqas an issue.. wipe it.. and resync..
				MinimaLogger.log("ERROR loading ArchiveDB.. WIPE and RESYNC.. ");
				
				//Close the DB
				mArchive.hardCloseDB();
				
				//Delete the ArchiveDB folder
				MiniFile.deleteFileOrFolder(archsqlfolder.getAbsolutePath(), archsqlfolder);
				
				//And reload..
				mArchive = new ArchiveManager();
				mArchive.loadDB(new File(archsqlfolder,"archive"));
			}
			
			//Are we Storing in a MySQL..
			if(!GeneralParams.MYSQL_HOST.equals("")) {
				mArchive.setupMySQL(
						GeneralParams.MYSQL_HOST, 
						GeneralParams.MYSQL_DB,
						GeneralParams.MYSQL_USER,
						GeneralParams.MYSQL_PASSWORD);
			}
			
			//Load the SQL DB
			File txpowsqlfolder = new File(basedb,"txpowsql");
			try {
				
				//Try and load the Archive DB
				mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
				
//				throw new Exception("TEST CRASH TXPOWDB!");
				
			}catch(Exception exc) {
				
				//Log the complete error
				MinimaLogger.log(exc);
				
				//There wqas an issue.. wipe it.. and resync..
				MinimaLogger.log("ERROR loading TxPoWSQLDB.. WIPE and RESYNC.. ");
				
				//Close the DB
				mTxPoWDB.hardCloseSQLDB();
				
				//Delete the ArchiveDB folder
				MiniFile.deleteFileOrFolder(txpowsqlfolder.getAbsolutePath(), txpowsqlfolder);
				
				//And reload..
				mTxPoWDB	= new TxPoWDB();
				mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
			}
			
			//Load the wallet
			File walletsqlfolder = new File(basedb,"walletsql");
			mWallet.loadDB(new File(walletsqlfolder,"wallet"));
			
			//Load the MaximaDB
			File maxsqlfolder = new File(basedb,"maximasql");
			mMaximaDB.loadDB(new File(maxsqlfolder,"maxima"));
			
			//Load ther MDS DB
			File mdssqlfolder = new File(basedb,"mdssql");
			mMDSDB.loadDB(new File(mdssqlfolder,"mds"));
			
			//Load the User Prefs
			mUserDB.loadDB(new File(basedb,"userprefs.db"));
			
			//Load the custom Txns..
			mTxnDB = new TxnDB();
			mTxnDB.loadDB();
			
			//Load the Cascade
			mCascade.loadDB(new File(basedb,"cascade.db"));
			
			//Load the TxPoWTree
			mTxPoWTree.loadDB(new File(basedb,"chaintree.db"));
			
			//And finally..
			mP2PDB.loadDB(new File(basedb,"p2p.db"));
			
			//Do we need to store the cascade in the ArchiveDB
			getArchive().checkCascadeRequired(getCascade());
			
		}catch(Exception exc) {
			MinimaLogger.log("SERIOUS ERROR loadAllDB ");
			MinimaLogger.log(exc);
			
			//At this point.. STOP..
			Runtime.getRuntime().halt(0);
//			System.exit(1);
		}
		
		//Release the krakken
		writeLock(false);
	}
	
	public void loadArchiveAndTxPoWDB(boolean zResetWallet) {
		
		//We need read lock 
		writeLock(true);
		
		try {
			
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//Set the Archive folder
			mArchive			= new ArchiveManager();
			File archsqlfolder 	= new File(basedb,"archivesql");
			mArchive.loadDB(new File(archsqlfolder,"archive"));
			
			//Are we Storing in a MySQL..
			if(!GeneralParams.MYSQL_HOST.equals("")) {
				mArchive.setupMySQL(
						GeneralParams.MYSQL_HOST, 
						GeneralParams.MYSQL_DB,
						GeneralParams.MYSQL_USER,
						GeneralParams.MYSQL_PASSWORD);
			}
			
			//Load the SQL DB
			mTxPoWDB			= new TxPoWDB();
			File txpowsqlfolder = new File(basedb,"txpowsql");
			mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
			
			//Wallet
			if(zResetWallet) {
				mWallet					= new Wallet();
				File walletsqlfolder 	= new File(basedb,"walletsql");
				mWallet.loadDB(new File(walletsqlfolder,"wallet"));
			}
			
		}catch(Exception exc) {
			MinimaLogger.log("SERIOUS ERROR loadArchiveAndTxPoWDB");
			MinimaLogger.log(exc);
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
		
		//We need lock 
		writeLock(true);
		
		try {
			//Clean shutdown of SQL DBs
			MinimaLogger.log("TxPowDB shutdown..");
			mTxPoWDB.saveDB();
			MinimaLogger.log("ArchiveDB shutdown..");
			mArchive.saveDB();
			MinimaLogger.log("Wallet shutdown..");
			mWallet.saveDB();
			MinimaLogger.log("Maxima shutdown..");
			mMaximaDB.saveDB();
			MinimaLogger.log("MDSDB shutdown..");
			mMDSDB.saveDB();
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		writeLock(false);
	}
	
	public void saveWalletSQL() {
		
		//We need lock 
		writeLock(true);
		
		try {
			mWallet.saveDB();
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		writeLock(false);
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
			mCascade.saveDB(new File(basedb,"cascade.db"));
			mTxPoWTree.saveDB(new File(basedb,"chaintree.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		readLock(false);
	}
	
	public void saveUserDB() {
		
		//We need read lock 
		readLock(true);
		
		try {
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//JsonDBs
			mUserDB.saveDB(new File(basedb,"userprefs.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		readLock(false);
	}
}
