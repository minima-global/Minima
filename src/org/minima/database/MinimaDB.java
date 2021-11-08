package org.minima.database;

import java.io.File;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.userprefs.UserDB;
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
		if(mMinimaDB == null) {
			mMinimaDB = new MinimaDB();
		}
	}
	public static void clearDB() {
		MinimaLogger.log("MinimaDB cleared..");
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
	Wallet			mWallet;
	
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
	
	public UserDB getUserDB() {
		return mUserDB;
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
		return new File(GeneralParams.CONFIGURATION_FOLDER,"databases");
	}
	
	public void loadAllDB() {
		//Get the base Database folder
		File basedb = getBaseDBFolder();
		
		//Set the Archive folder
		File archsqlfolder = new File(basedb,"archivesql");
		mArchive.loadDB(new File(archsqlfolder,"archive"));
		
		//Load the wallet
		File walletsqlfolder = new File(basedb,"walletsql");
		mWallet.loadDB(new File(walletsqlfolder,"wallet"));
		
		//Load the SQL DB
		File txpowsqlfolder = new File(basedb,"txpowsql");
		mTxPoWDB.loadSQLDB(new File(txpowsqlfolder,"txpow"));
		
		//Load the User Prefs
		mUserDB.loadDB(new File(basedb,"userprefs.db"));
		
		//Load the Cascade
		mCacscade.loadDB(new File(basedb,"cascade.db"));
		
		//Load the TxPoWTree
		mTxPoWTree.loadDB(new File(basedb,"chaintree.db"));
		
		//And finally..
		mP2PDB.loadDB(new File(basedb,"p2p.db"));
	}
	
	public void saveAllDB() {
		//Get the base Database folder
		File basedb = getBaseDBFolder();
		
		//Clean shutdown of SQL DBs
		mTxPoWDB.saveDB();
		mArchive.saveDB();
		mWallet.saveDB();
		
		//JsonDBs
		mUserDB.saveDB(new File(basedb,"userprefs.db"));
		mP2PDB.saveDB(new File(basedb,"p2p.db"));
		
		//Custom
		mCacscade.saveDB(new File(basedb,"cascade.db"));
		mTxPoWTree.saveDB(new File(basedb,"chaintree.db"));
	}
	
	public void saveState() {
		
		//We need read lock 
		readLock(true);
		
		try {
			//Get the base Database folder
			File basedb = getBaseDBFolder();
			
			//JsonDBs
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
