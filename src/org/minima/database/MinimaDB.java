package org.minima.database;

import java.io.File;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.minima.database.archive.ArchiveManager;
import org.minima.database.archive.TxBlockDB;
import org.minima.database.cascade.Cascade;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.mmr.MegaMMR;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.userprefs.UserDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.wallet.Wallet;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.network.p2p.P2PDB;
import org.minima.system.network.p2p2.P2P2DB;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.TimerMessage;

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
	TxBlockDB		mTxBlockDB;
	
	/**
	 * The MEGA MMR
	 */
	MegaMMR mMegaMMR;
	
	/**
	 * For P2P Information
	 */
	P2PDB			mP2PDB;
	P2P2DB			mP2P2DB;
	
	/**
	 * LOCKING the MinimaDB for read write operations..
	 */
	ReentrantReadWriteLock mRWLock;
	public static String  mCurrentWriteLockThread = "";
	public static boolean mCurrentWriteLockState  = false;
	
	/**
	 * The coin Addresses to Notify
	 */
	HashSet<String> mCoinNotify;
	
	/**
	 * Do we allow save state
	 */
	boolean mAllowSaveState = true;
	
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
		mTxBlockDB	= new TxBlockDB();
		
		//The P2P
		mP2PDB		= new P2PDB();
		mP2P2DB		= new P2P2DB();
		
		mRWLock 	= new ReentrantReadWriteLock();
		
		mCoinNotify	= new HashSet<>();
		
		mMegaMMR	= new MegaMMR(GeneralParams.MEGAMMR_MEGAPRUNE);
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
			
			//Try and unlock this READ lock
			try {
				mRWLock.readLock().unlock();
			}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
		}
	}
	
	public void writeLock(boolean zLock) {
		
		if(zLock) {
			mRWLock.writeLock().lock();
			
			//Which thread is this..
			mCurrentWriteLockThread = Thread.currentThread().getName();
			
		}else {
			mRWLock.writeLock().unlock();
		}
		
		mCurrentWriteLockState  = zLock;
	}
	
	public void safeReleaseWriteLock() {
		//Release it if held by this thread..
		if(mRWLock.writeLock().isHeldByCurrentThread()) {
			mRWLock.writeLock().unlock();
		}
	}
	
	public String getRWLockInfo() {
		return mRWLock.toString();
	}
	
	/**
	 * Get the various different databases
	 */
	public TxPoWDB getTxPoWDB() {
		return mTxPoWDB;
	}
	
	public TxBlockDB getTxBlockDB() {
		return mTxBlockDB;
	}
	
	public TxPowTree getTxPoWTree() {
		return mTxPoWTree;
	}
	
	public MegaMMR getMegaMMR() {
		return mMegaMMR;
	}
	
	public void hardSetMegaMMR(MegaMMR zMEGA) {
		mMegaMMR = zMEGA;
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
	
	public File getCascadeFile() {
		return getDBFile("cascade.db");
	}
	
	public File getDBFile(String zFilename) {
		//Get the base Database folder
		File basedb = getBaseDBFolder();
		
		//The File
		return new File(basedb,zFilename);
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
	
	public P2P2DB getP2P2DB() {
		return mP2P2DB;
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
			
			//Load the wallet
			if(Main.STARTUP_DEBUG_LOGS) {
				MinimaLogger.log("MinimaDB load Wallet..");
			}
			File walletsqlfolder = new File(basedb,"walletsql");
			if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
				mWallet.loadDB(new File(walletsqlfolder,"wallet"));
			}else {
				MinimaLogger.log("Using Encrypted SQL DB");
				mWallet.loadEncryptedSQLDB(new File(walletsqlfolder,"wallet"),GeneralParams.MAIN_DBPASSWORD);
			}
			
			//Set the Archive folder
			if(Main.STARTUP_DEBUG_LOGS) {
				MinimaLogger.log("MinimaDB load ArchiveDB..");
			}
			File archsqlfolder = new File(basedb,"archivesql");
			try {
				
				//Try and load the Archive DB
				mArchive.loadDB(new File(archsqlfolder,"archive"));
				
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
			
			//Load the SQL DB
			if(Main.STARTUP_DEBUG_LOGS) {
				MinimaLogger.log("MinimaDB load TxPoWDB..");
			}
			File txpowsqlfolder = new File(basedb,"txpowsql");
			try {
				
				//Try and load the Archive DB
				mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
				
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
			
			//Load the Maxima DB
			if(Main.STARTUP_DEBUG_LOGS) {
				MinimaLogger.log("MinimaDB load MaximaDB..");
			}
			File maxsqlfolder = new File(basedb,"maximasql");
			try {
				
				if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
					mMaximaDB.loadDB(new File(maxsqlfolder,"maxima"));
				}else {
					mMaximaDB.loadEncryptedSQLDB(new File(maxsqlfolder,"maxima"),GeneralParams.MAIN_DBPASSWORD);
				}
				
			}catch(Exception exc) {
				
				//Log the complete error
				MinimaLogger.log(exc);
				
				//There wqas an issue.. wipe it.. and resync..
				MinimaLogger.log("ERROR loading MaximaDB.. WIPE and RESTART.. ");
				
				//Close the DB
				mMaximaDB.hardCloseDB();
				
				//Delete the ArchiveDB folder
				MiniFile.deleteFileOrFolder(maxsqlfolder.getAbsolutePath(), maxsqlfolder);
				
				//And reload..
				mMaximaDB	= new MaximaDB();
				if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
					mMaximaDB.loadDB(new File(maxsqlfolder,"maxima"));
				}else {
					mMaximaDB.loadEncryptedSQLDB(new File(maxsqlfolder,"maxima"),GeneralParams.MAIN_DBPASSWORD);
				}
			}
			
			//Load the MDS DB
			File mdssqlfolder = new File(basedb,"mdssql");
			if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
				mMDSDB.loadDB(new File(mdssqlfolder,"mds"));
			}else {
				mMDSDB.loadEncryptedSQLDB(new File(mdssqlfolder,"mds"),GeneralParams.MAIN_DBPASSWORD);
			}
			
			//Load the User Prefs
//			mUserDB.loadEncryptedDB(GeneralParams.MAIN_DBPASSWORD, new File(basedb,"userprefs.db"));
			mUserDB.loadDB(new File(basedb,"userprefs.db"));
			
			//Load the custom Txns..
			mTxnDB = new TxnDB();
			mTxnDB.loadDB();
			
			//Load the Cascade
			mCascade.loadDB(new File(basedb,"cascade.db"));
			
			//Load the TxPoWTree
			File txtree = new File(basedb,"chaintree.db");
			MinimaLogger.log("Loading TxPowTree size : "+MiniFormat.formatSize(txtree.length()));
			mTxPoWTree.loadDB(new File(basedb,"chaintree.db"));
			
			//Load P2P DB
//			mP2PDB.loadEncryptedDB(GeneralParams.MAIN_DBPASSWORD, new File(basedb,"p2p.db"));
			mP2PDB.loadDB(new File(basedb,"p2p.db"));
			mP2P2DB.loadDB(new File(basedb,"p2p2.db"));
			
			//Check YOUR cascade..
			if(!Cascade.checkCascadeCorrect(mCascade)) {
				throw new Exception("Your Cascade is BROKEN.. please 'reset' your node.");
			}
		
			//Are we running in MEGA MMR mode..
			if(GeneralParams.IS_MEGAMMR) {
				mMegaMMR.loadMMR(new File(basedb,"megammr.mmr"));
			}else {
				//Delete if exists..
				MiniFile.deleteFileOrFolder(basedb.getAbsolutePath() , new File(basedb,"megammr.mmr"));
			}

			//And check it ends where the tree ends..
			if(mCascade.getTip() != null && mTxPoWTree.getRoot()!=null) {
				MiniNumber cascstart = mCascade.getTip().getTxPoW().getBlockNumber();
				MiniNumber treeroot  = mTxPoWTree.getRoot().getTxPoW().getBlockNumber();
				if(!treeroot.isEqual(cascstart.increment())) {
					throw new Exception("Your Cascade is BROKEN.. please 'reset' your node.");
				}
				
				//Check the MEGA MMR starts on the correct block
				if(GeneralParams.IS_MEGAMMR && !mMegaMMR.isEmpty()) {
					if(!mMegaMMR.getMMR().getBlockTime().isEqual(treeroot.decrement())) {
						throw new Exception("Your MEGAMMR is BROKEN "
								+ "(does not start "+mMegaMMR.getMMR().getBlockTime()+" where "
										+ "tree ends "+treeroot+").. please 'reset' your node.");
					}
				}
			}
			
			//Clean Mem after that
			System.gc();
			
			//Do we need to store the cascade in the ArchiveDB
			getArchive().checkCascadeRequired(getCascade());
			
		}catch(Exception exc) {
			MinimaLogger.log("SERIOUS ERROR loadAllDB : ");
			MinimaLogger.log(exc);
			
			//Do we have a rescue NODE
			String err = exc.toString();
			if(!GeneralParams.RESCUE_MEGAMMR_NODE.equals("")) {
				
				MinimaLogger.log("RESCUE NODE FOUND.. attempting rescue @ "+GeneralParams.RESCUE_MEGAMMR_NODE);
				
				//Post a message that does a RESCUE..
				Main.getInstance().PostTimerMessage(new TimerMessage(1000, Main.MAIN_DO_RESCUE));
				
			}else {
				
				//Are we on mobile or JNLP
				if(GeneralParams.IS_MOBILE || GeneralParams.IS_JNLP) {
				
					//Set this param
					Main.getInstance().setStartUpError(true, err);
					
				}else {
					//At this point.. STOP..
					Runtime.getRuntime().halt(0);
				}
			}
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
			
			//Wallet
			if(zResetWallet) {
				mWallet					= new Wallet();
				File walletsqlfolder 	= new File(basedb,"walletsql");
				if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
					mWallet.loadDB(new File(walletsqlfolder,"wallet"));
				}else {
					mWallet.loadEncryptedSQLDB(new File(walletsqlfolder,"wallet"),GeneralParams.MAIN_DBPASSWORD);
				}
			}
			
			//Set the Archive folder
			mArchive			= new ArchiveManager();
			File archsqlfolder 	= new File(basedb,"archivesql");
			mArchive.loadDB(new File(archsqlfolder,"archive"));
			
			//Load the SQL DB
			mTxPoWDB			= new TxPoWDB();
			File txpowsqlfolder = new File(basedb,"txpowsql");
			mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
			
		}catch(Exception exc) {
			MinimaLogger.log("SERIOUS ERROR loadArchiveAndTxPoWDB");
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		writeLock(false);
	}
	
	public void loadDBsForRestoreSync() {
		
		//We need read lock 
		writeLock(true);
		
		try {
			
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//Wallet
			mWallet					= new Wallet();
			File walletsqlfolder 	= new File(basedb,"walletsql");
			if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
				mWallet.loadDB(new File(walletsqlfolder,"wallet"));
			}else {
				mWallet.loadEncryptedSQLDB(new File(walletsqlfolder,"wallet"),GeneralParams.MAIN_DBPASSWORD);
			}
			
			//Set the Archive folder
			mArchive			= new ArchiveManager();
			File archsqlfolder 	= new File(basedb,"archivesql");
			mArchive.loadDB(new File(archsqlfolder,"archive"));
			
			//Load the SQL DB
			mTxPoWDB			= new TxPoWDB();
			File txpowsqlfolder = new File(basedb,"txpowsql");
			mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
			
			//Load the Cascade
			mCascade = new Cascade();
			mCascade.loadDB(new File(basedb,"cascade.db"));
			
			//Load the TxPoWTree
			mTxPoWTree = new TxPowTree();
			mTxPoWTree.loadDB(new File(basedb,"chaintree.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log("SERIOUS ERROR loadDBsForRestoreSync");
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		writeLock(false);
	}
	
	public void saveAllDB() {
		saveAllDB(false);
	}
	
	public void saveAllDB(boolean zCompact) {
		MinimaLogger.log("Saving State..");
		saveState();
		
		MinimaLogger.log("Saving SQL..");
		saveSQL(zCompact);
		
		MinimaLogger.log("All saved..");
	}
	
	public void saveSQL(boolean zCompact) {
		
		//We need lock 
		writeLock(true);
		
		try {
	
			//Clean shutdown of SQL DBs
			MinimaLogger.log("Wallet shutdown..");
			mWallet.saveDB(true);
			MinimaLogger.log("Maxima shutdown..");
			mMaximaDB.saveDB(zCompact);
			MinimaLogger.log("MDSDB shutdown..");
			mMDSDB.saveDB(zCompact);
			MinimaLogger.log("TxPowDB shutdown..");
			mTxPoWDB.saveDB(zCompact);
			MinimaLogger.log("ArchiveDB shutdown..");
			mArchive.saveDB(zCompact);
			
			MinimaLogger.log("All SQL DB Shutdown..");
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
			
		}finally {
			//Release the krakken
			writeLock(false);
		}
	}
	
	public void fullDBRestartMemFree() {
		
		//We need lock 
		writeLock(true);
				
		try {
			
			//MinimaLogger.log("Memory clear started..");
			
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//Wipe the old data..
			mTxPoWDB.wipeDBRAM();
			mTxPoWDB.getSQLDB().cleanDB(true);
			mTxPoWDB.getOnChainDB().cleanDB(true);
			mArchive.checkForCleanDB();
			
			//Shut them down
			//MinimaLogger.log("Save TxPoWDB..");
			mTxPoWDB.saveDB(false);
			//MinimaLogger.log("Save ArchiveDB..");
			mArchive.saveDB(false);
			//MinimaLogger.log("Save WalletDB..");
			mWallet.saveDB(false);
			
			//MinimaLogger.log("Load DBs..");
			
			//Wallet
			mWallet					= new Wallet();
			File walletsqlfolder 	= new File(basedb,"walletsql");
			if(!GeneralParams.IS_MAIN_DBPASSWORD_SET) {
				mWallet.loadDB(new File(walletsqlfolder,"wallet"));
			}else {
				mWallet.loadEncryptedSQLDB(new File(walletsqlfolder,"wallet"),GeneralParams.MAIN_DBPASSWORD);
			}
			
			//Set the Archive folder
			mArchive			= new ArchiveManager();
			File archsqlfolder 	= new File(basedb,"archivesql");
			mArchive.loadDB(new File(archsqlfolder,"archive"));
			
			//Load the SQL DB
			mTxPoWDB			= new TxPoWDB();
			File txpowsqlfolder = new File(basedb,"txpowsql");
			mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
			
			//MinimaLogger.log("Memory clear finished..");
			
		} catch (SQLException e) {
			e.printStackTrace();
		}
		
		//Release the krakken
		writeLock(false);
	}
	
	public void setAllowSaveState(boolean zAllow) {
		mAllowSaveState = zAllow;
	}
	
	public void saveState() {
		
		//Are we allowed..
		if(!mAllowSaveState) {
			return; 
		}
		
		//We need read lock 
		readLock(true);
		
		try {
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//JsonDBs
			mTxnDB.saveDB();
//			mUserDB.saveEncryptedDB(GeneralParams.MAIN_DBPASSWORD, new File(basedb,"userprefs.db"));
//			mP2PDB.saveEncryptedDB(GeneralParams.MAIN_DBPASSWORD, new File(basedb,"p2p.db"));
			
			//MinimaLogger.log("SAVESTATE USERDB:"+mUserDB.getAllData().toString());
			mUserDB.saveDB(new File(basedb,"userprefs.db"));
			mP2PDB.saveDB(new File(basedb,"p2p.db"));
			mP2P2DB.saveDB(new File(basedb,"p2p2.db"));
			
			//Cascade
			mCascade.saveDB(new File(basedb,"cascade.db"));
			
			//TxPoWTree
			mTxPoWTree.saveDB(new File(basedb,"chaintree.db"));
			
			//And are we MEGA MMR..
			if(GeneralParams.IS_MEGAMMR) {
				mMegaMMR.saveMMR(new File(basedb,"megammr.mmr"));
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		readLock(false);
	}
	
	public void saveUserDB() {
		
		//Are we allowed..
		if(!mAllowSaveState) {
			return; 
		}
		
		//We need read lock 
		readLock(true);
		
		try {
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//JsonDBs
//			mUserDB.saveEncryptedDB(GeneralParams.MAIN_DBPASSWORD, new File(basedb,"userprefs.db"));
			
			//MinimaLogger.log("SAVEUSERDB USERDB:"+mUserDB.getAllData().toString());
			mUserDB.saveDB(new File(basedb,"userprefs.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		readLock(false);
	}
	
	public void saveP2PDB() {
		
		//Are we allowed..
		if(!mAllowSaveState) {
			return; 
		}
		
		//We need read lock 
		//readLock(true);
		
		try {
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//JsonDBs
			mP2PDB.saveDB(new File(basedb,"p2p.db"));
			mP2P2DB.saveDB(new File(basedb,"p2p2.db"));
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Release the krakken
		//readLock(false);
	}
	
	/**
	 * Coin Notification
	 * 
	 * Different to tracking as can be any coin without the script
	 */
	public void addCoinNotify(String zAddress) {
		mCoinNotify.add(zAddress);
	}
	
	public boolean removeCoinNotify(String zAddress) {
		return mCoinNotify.remove(zAddress);
	}
	
	public boolean checkCoinNotify(String zAddress) {
		return mCoinNotify.contains(zAddress);
	}
}
