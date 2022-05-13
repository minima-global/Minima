package org.minima.database.wallet;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Random;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class Wallet extends SqlDB {

	/**
	 * How many default keys to create 
	 */
	public static int NUMBER_GETADDRESS_KEYS = 64;
	
	/**
	 * The MAIN Private seed from which all others are derived..
	 */
	private MiniData mMainPrivateSeed = MiniData.ZERO_TXPOWID; 
	
	/**
	 * Key SQL
	 */
	PreparedStatement SQL_CREATE_PUBLIC_KEY 			= null;
	PreparedStatement SQL_GET_KEY 						= null;
	PreparedStatement SQL_GET_ALL_KEYS 					= null;
	PreparedStatement SQL_UPDATE_KEY_USES 				= null;
	
	PreparedStatement SQL_WIPE_PRIVATE_KEYS 			= null;
	PreparedStatement SQL_UPDATE_PRIVATE_KEYS 			= null;
	
	/**
	 * Scripts SQL
	 */
	PreparedStatement SQL_ADD_SCRIPT 			= null;
	PreparedStatement SQL_LIST_ALL_SCRIPTS 		= null;
	PreparedStatement SQL_LIST_SIMPLE_SCRIPTS 	= null;
	PreparedStatement SQL_LIST_TRACK_SCRIPTS 	= null;
	PreparedStatement SQL_LIST_DEFAULT_SCRIPTS 	= null;
	PreparedStatement SQL_GET_SCRIPT 			= null;
	
	/**
	 * Cached Lists of Data for fast O(1) checking..
	 */
	HashSet<String> mAllKeys 			= new HashSet<>();
	HashSet<String> mAllTrackedAddress 	= new HashSet<>();
	HashSet<String> mAllSimpleAddress 	= new HashSet<>();
	
	/**
	 * Stop creating keys if you are
	 */
	boolean mShuttingdown = false;
	
	public Wallet() {
		super();
		
		//Are we testing..
		if(GeneralParams.TEST_PARAMS) {
			NUMBER_GETADDRESS_KEYS = 8;
		}
	}
	
	public void shuttiongDown() {
		mShuttingdown = true;
	}
	
	private boolean isShuttingDown() {
		return mShuttingdown;
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();
			
			//Create keys table
			String createkeys = "CREATE TABLE IF NOT EXISTS `keys` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `size` int NOT NULL,"
							+ "  `depth` int NOT NULL,"
							+ "  `uses` bigint NOT NULL,"
							+ "  `maxuses` bigint NOT NULL,"
							+ "  `modifier` varchar(80) NOT NULL,"
							+ "  `privatekey` varchar(80) NOT NULL,"
							+ "  `publickey` varchar(80) NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(createkeys);
			
			//Create scripts table
			String scriptsdb = "CREATE TABLE IF NOT EXISTS `scripts` ("
							 + "  `id` IDENTITY PRIMARY KEY,"
							 + "  `script` varchar(8192) NOT NULL,"
							 + "  `address` varchar(80) NOT NULL,"
							 + "  `simple` int NOT NULL,"
							 + "  `defaultaddress` int NOT NULL,"
							 + "  `publickey` varchar(80) NOT NULL,"
							 + "  `track` int NOT NULL"
							 + ")";
			
			//Run it..
			stmt.execute(scriptsdb);
			
			//All done..
			stmt.close();
			
			//KEY functions
			SQL_CREATE_PUBLIC_KEY 			= mSQLConnection.prepareStatement("INSERT IGNORE INTO keys ( size, depth, uses, maxuses, modifier, privatekey, publickey ) VALUES ( ?, ?, ?, ? ,? ,? ,? )");
			SQL_GET_ALL_KEYS				= mSQLConnection.prepareStatement("SELECT * FROM keys");
			SQL_GET_KEY						= mSQLConnection.prepareStatement("SELECT * FROM keys WHERE publickey=?");
			SQL_UPDATE_KEY_USES				= mSQLConnection.prepareStatement("UPDATE keys SET uses=? WHERE publickey=?");
			
			//Base Seed functions
			SQL_WIPE_PRIVATE_KEYS			= mSQLConnection.prepareStatement("UPDATE keys SET privatekey='0x00' WHERE privatekey!='0x00'");
			SQL_UPDATE_PRIVATE_KEYS			= mSQLConnection.prepareStatement("UPDATE keys SET privatekey=? WHERE publickey=?");
			
			//ScriptsDB
			SQL_ADD_SCRIPT				= mSQLConnection.prepareStatement("INSERT IGNORE INTO scripts ( script, address, simple, defaultaddress, publickey, track ) VALUES ( ? , ? , ? , ? , ? , ? )");
			SQL_LIST_ALL_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts");
			SQL_LIST_SIMPLE_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE simple<>0");
			SQL_LIST_TRACK_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE track<>0");
			SQL_LIST_DEFAULT_SCRIPTS	= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE defaultaddress<>0");
			SQL_GET_SCRIPT				= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE address=?");
			
			//Now load up the caches..
			ArrayList<KeyRow> allkeys = getAllKeys();
			for(KeyRow key : allkeys) {
				mAllKeys.add(key.getPublicKey());
			}
			
			ArrayList<ScriptRow> allscripts = getAllAddresses();
			for(ScriptRow srow : allscripts) {
				String address = srow.getAddress();
				if(srow.isTrack()) {
					mAllTrackedAddress.add(address);
				}
				if(srow.isSimple()) {
					mAllSimpleAddress.add(address);
				}
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
	/**
	 * The BASE seed is used to generate all the keys..
	 */
	
	public MiniData getBaseSeed() {
		return mMainPrivateSeed;
	}
	
	public MiniData initBaseSeed(MiniData zBaseSeed) {
		return mMainPrivateSeed = zBaseSeed;
	}
	
	public boolean isBaseSeedAvailable() {
		return !mMainPrivateSeed.isEqual(MiniData.ZERO_TXPOWID);
	}
	
	public void wipeBaseSeed() throws SQLException {
		//Wipe the DB
		SQL_WIPE_PRIVATE_KEYS.execute();
		
		//reset the base seed
		mMainPrivateSeed = MiniData.ZERO_TXPOWID;
	}
	
	public boolean resetBaseSeed(MiniData zBaseSeed) {
		
		//reset the base seed
		mMainPrivateSeed = zBaseSeed;
						
		//Get all the keys..
		ArrayList<KeyRow> keys = getAllKeys();
		
		try {
			
			//Now cycle through..
			for(KeyRow key : keys) {
				
				//Get the modifier..
				MiniData modifier = new MiniData(key.getModifier());
			
				//Now create a random private seed using the modifier
				MiniData privseed 	= Crypto.getInstance().hashObjects(zBaseSeed, modifier);
				
				//And now update the DB..
				SQL_UPDATE_PRIVATE_KEYS.clearParameters();
				SQL_UPDATE_PRIVATE_KEYS.setString(1, privseed.to0xString());
				SQL_UPDATE_PRIVATE_KEYS.setString(2, key.getPublicKey());
				SQL_UPDATE_PRIVATE_KEYS.execute();
			}
		
		} catch (SQLException e) {
			MinimaLogger.log(e);
			
			return false;
		}
		
		return true;
	}
	
	/**
	 * Create an initial set of keys / addresses to use
	 */
	public boolean initDefaultKeys() {
		
		//Get all the keys..
		ArrayList<ScriptRow> allscripts = getAllDefaultAddresses();
		boolean allcreated = false;
		
		//Check we have the desired amount..
		int numkeys = allscripts.size();
		if(numkeys < NUMBER_GETADDRESS_KEYS) {
			
			//Create a few at a time..
			int diff = NUMBER_GETADDRESS_KEYS - numkeys;
			if(diff>8) {
				diff = 8;
			}
			
			//Create the keys
			for(int i=0;i<diff;i++) {
				if(!isShuttingDown()) {
					createNewSimpleAddress(true);
				}
			}
			
			MinimaLogger.log(diff+" more initial keys created.. Total now : "+(numkeys+diff));
		}else {
			allcreated = true;
		}
		
		return allcreated;
	}
	
	/**
	 * Get 1 of your default addresses
	 */
	public ScriptRow getDefaultAddress() {
		//Get all the default addresses
		ArrayList<ScriptRow> allkeys = getAllDefaultAddresses();
		int numkeys = allkeys.size();
		
		//Now pick a random key..
		int rand = new Random().nextInt(numkeys);
		
		return allkeys.get(rand);
	}
	
	/**
	 * Create a NEW Simple Address
	 */
	public ScriptRow createNewSimpleAddress(boolean zDefault) {
		
		//Now create a new KEY
		KeyRow key = createNewKey();
		
		//Now create a simple address..
		String script = new String("RETURN SIGNEDBY("+key.getPublicKey()+")");
		
		//Now add to the database..
		return addScript(script, true, zDefault, key.getPublicKey(), true);
	}
	
	/**
	 * Create a NEW key
	 */
	public synchronized KeyRow createNewKey() {
		
		//Check we can create new keys
		if(!isBaseSeedAvailable()) {
			throw new IllegalArgumentException("KeysDB LOCKED. Base SEED missing..");
		}
		
		//Create a random modifier..
		MiniData modifier 	= MiniData.getRandomData(32);
		
		//Now create a random private seed using the modifier
		MiniData privseed 	= Crypto.getInstance().hashObjects(mMainPrivateSeed, modifier);
		
		//Make the TreeKey
		TreeKey treekey 	= TreeKey.createDefault(privseed);
		
		//Now put all this in the DB
		try {
			
			//Get the Query ready
			SQL_CREATE_PUBLIC_KEY.clearParameters();
		
			//Set main params
			SQL_CREATE_PUBLIC_KEY.setInt(1, treekey.getSize()); // FOR NOW.. not based off seed phrase
			SQL_CREATE_PUBLIC_KEY.setInt(2, treekey.getDepth());
			SQL_CREATE_PUBLIC_KEY.setInt(3, treekey.getUses());
			SQL_CREATE_PUBLIC_KEY.setInt(4, treekey.getMaxUses());
			
			//NULL Modifier for now..
			SQL_CREATE_PUBLIC_KEY.setString(5, modifier.to0xString());
			
			SQL_CREATE_PUBLIC_KEY.setString(6, treekey.getPrivateKey().to0xString());
			SQL_CREATE_PUBLIC_KEY.setString(7, treekey.getPublicKey().to0xString());
			
			//Do it.
			SQL_CREATE_PUBLIC_KEY.execute();
			
			//Add this Key to our Cache List
			mAllKeys.add(treekey.getPublicKey().to0xString());
			
			return new KeyRow(	treekey.getSize(), treekey.getDepth(), 
								treekey.getUses(), treekey.getMaxUses(), "0x00", 
								treekey.getPrivateKey().to0xString(), 
								treekey.getPublicKey().to0xString());
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	/**
	 * Add a custom script
	 */
	public synchronized ScriptRow addScript(String zScript, boolean zSimple, boolean zDefault, String zPublicKey, boolean zTrack) {
		
		//Create the address
		String addr = new Address(zScript).getAddressData().to0xString();
		
		//Now put all this in the DB
		try {
		
			//Get the Query ready
			SQL_ADD_SCRIPT.clearParameters();
		
			//Set main params
			SQL_ADD_SCRIPT.setString(1, zScript);
			SQL_ADD_SCRIPT.setString(2, addr);
			
			//Do we track ALL these addresses..
			if(zSimple) {
				SQL_ADD_SCRIPT.setInt(3, 1);
			}else {
				SQL_ADD_SCRIPT.setInt(3, 0);
			}
			
			if(zDefault) {
				SQL_ADD_SCRIPT.setInt(4, 1);
			}else {
				SQL_ADD_SCRIPT.setInt(4, 0);
			}
			
			SQL_ADD_SCRIPT.setString(5, zPublicKey);
			
			if(zTrack) {
				SQL_ADD_SCRIPT.setInt(6, 1);
			}else {
				SQL_ADD_SCRIPT.setInt(6, 0);
			}
			
			//Do it.
			SQL_ADD_SCRIPT.execute();
			
			//Add to our Cache List for fast checking
			if(zTrack) {
				mAllTrackedAddress.add(addr);
			}
			
			if(zSimple) {
				mAllSimpleAddress.add(addr);
			}
			
			return new ScriptRow(zScript, addr, zSimple, zDefault, zPublicKey, zTrack);
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}

	/**
	 * Get the ScriptRow for an address
	 */
	public synchronized ScriptRow getScriptFromAddress(String zAddres) {
		try {
			
			//Get the Query ready
			SQL_GET_SCRIPT.clearParameters();
		
			//Set main params
			SQL_GET_SCRIPT.setString(1, zAddres);
			
			//Run the query
			ResultSet rs = SQL_GET_SCRIPT.executeQuery();
			
			//Could be multiple results
			if(rs.next()) {
				return new ScriptRow(rs);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}

	
	/**
	 * Is this KEY one of our keys
	 */
	public synchronized boolean isKeyRelevant(String zPublicKey) {
		boolean ourkey = mAllKeys.contains(zPublicKey);
//		MinimaLogger.log("[WALLET] isKeyRelevant : "+zPublicKey+" "+ourkey);
		return ourkey;
	}
	
	/**
	 * Is this ADDRESS relevant to us - are we tracking it
	 */
	public synchronized boolean isAddressRelevant(String zAddress) {
		boolean tracked = mAllTrackedAddress.contains(zAddress);
//		MinimaLogger.log("[WALLET] isAddressRelevant : "+zAddress+" "+tracked);
		return tracked;
	}
	
	/**
	 * Is this ADDRESS a simple address
	 */
	public synchronized boolean isAddressSimple(String zAddress) {
		boolean simple = mAllSimpleAddress.contains(zAddress);
//		MinimaLogger.log("[WALLET] isAddressSimple : "+zAddress+" "+simple);
		return simple;
	}
	
	/**
	 * Get all Keys 
	 */
	public synchronized ArrayList<KeyRow> getAllKeys() {
		
		ArrayList<KeyRow> allkeys = new ArrayList<>();
		try {
			
			//Run the query
			ResultSet rs = SQL_GET_ALL_KEYS.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the details
				KeyRow scrow = new KeyRow(rs);
				
				//Add to our list
				allkeys.add(scrow);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return allkeys;
	}
	
	/**
	 * Get KeyRow from Public Key
	 */
	public synchronized KeyRow getKeyFromPublic(String zPublicKey) {
		try {
			
			//Get the Query ready
			SQL_GET_KEY.clearParameters();
		
			//Set main params
			SQL_GET_KEY.setString(1, zPublicKey);
			
			//Run the query
			ResultSet rs = SQL_GET_KEY.executeQuery();
			
			//Could be multiple results
			if(rs.next()) {
				return new KeyRow(rs);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	/**
	 * Get all relevant Public Keys and Addresses
	 */
	public synchronized ArrayList<ScriptRow> getAllAddresses() {
		
		//Do both sets..
		ArrayList<ScriptRow> allscripts = new ArrayList<>();
		try {
			
			//Run the query
			ResultSet rs = SQL_LIST_ALL_SCRIPTS.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the details
				ScriptRow scrow = new ScriptRow(rs);
				
				//Add to our list
				allscripts.add(scrow);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return allscripts;
	}
	
	private synchronized ArrayList<ScriptRow> getAllDefaultAddresses() {
		
		//Do both sets..
		ArrayList<ScriptRow> allscripts = new ArrayList<>();
		try {
			
			//Run the query
			ResultSet rs = SQL_LIST_DEFAULT_SCRIPTS.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the details
				ScriptRow scrow = new ScriptRow(rs);
				
				//Add to our list
				allscripts.add(scrow);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return allscripts;
	}
	
	/**
	 * Sign a piece of data with a specific public key
	 */
	public Signature signData(String zPublicKey, MiniData zData) {
		
		try {
			
			//First get this KeyRow..
			SQL_GET_KEY.clearParameters();
		
			//Set main params
			SQL_GET_KEY.setString(1, zPublicKey);
			
			//Run the query
			ResultSet rs = SQL_GET_KEY.executeQuery();
			
			//Could be multiple results
			KeyRow key = null;
			if(rs.next()) {
				key = new KeyRow(rs);
			}else {
				return null;
			}
			
			//Create a new TreeKey
			TreeKey tk = new TreeKey(new MiniData(key.getPrivateKey()), key.getSize(), key.getDepth());
			
			//How many times has this been used.. get from DB
			int uses = key.getUses();
			
			//Set this..
			tk.setUses(uses);
			
			//Now we have the Key..
			Signature signature = tk.sign(zData);
			
			//Update the DB..
			uses = tk.getUses();
			
			//Update the DB..
			updateUses(uses, key.getPublicKey());
			
			//And finally return the sig..
			return signature;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
		
	private void updateUses(int zUses, String zPublicKey) throws SQLException {		

		//Get the Query ready
		SQL_UPDATE_KEY_USES.clearParameters();
	
		//Set main params
		SQL_UPDATE_KEY_USES.setInt(1, zUses);
		SQL_UPDATE_KEY_USES.setString(2, zPublicKey);
		
		//Run the query
		SQL_UPDATE_KEY_USES.execute();
	}
}
