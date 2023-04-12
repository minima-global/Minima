package org.minima.database.wallet;

import java.math.BigInteger;
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
import org.minima.system.commands.send.multisig;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
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
	private SeedRow mBaseSeed = null;
	
	/**
	 * Key SQL
	 */
	PreparedStatement SQL_CREATE_PUBLIC_KEY 			= null;
	PreparedStatement SQL_GET_KEY 						= null;
	PreparedStatement SQL_GET_ALL_KEYS 					= null;
	PreparedStatement SQL_UPDATE_KEY_USES 				= null;
	PreparedStatement SQL_UPDATE_ALL_KEY_USES 			= null;
	PreparedStatement SQL_UPDATE_INC_ALL_KEY_USES 		= null;
	
	PreparedStatement SQL_WIPE_PRIVATE_KEYS 			= null;
	PreparedStatement SQL_UPDATE_PRIVATE_KEYS 			= null;
	
	/**
	 * Scripts SQL
	 */
	PreparedStatement SQL_ADD_SCRIPT 			= null;
	PreparedStatement SQL_REMOVE_SCRIPT 		= null;
	PreparedStatement SQL_LIST_ALL_SCRIPTS 		= null;
	PreparedStatement SQL_LIST_SIMPLE_SCRIPTS 	= null;
	PreparedStatement SQL_LIST_TRACK_SCRIPTS 	= null;
	PreparedStatement SQL_LIST_DEFAULT_SCRIPTS 	= null;
	PreparedStatement SQL_GET_SCRIPT 			= null;
	
	/**
	 * Seed functions
	 */
	PreparedStatement SQL_SELECT_SEED 			= null;
	PreparedStatement SQL_INSERT_SEED 			= null;
	PreparedStatement SQL_UPDATE_SEED 			= null;
	
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
	
	public void shuttingDown() {
		mShuttingdown = true;
	}
	
	private boolean isShuttingDown() {
		return mShuttingdown;
	}
	
	@Override
	protected void createSQL() throws SQLException {
		
		//Create the various tables..
		Statement stmt = mSQLConnection.createStatement();
		
		//Create keys table
		String createkeys = "CREATE TABLE IF NOT EXISTS `keys` ("
						+ "  `id` bigint auto_increment,"
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
						 + "  `id` bigint auto_increment,"
						 + "  `script` varchar(8192) NOT NULL,"
						 + "  `address` varchar(80) NOT NULL,"
						 + "  `simple` int NOT NULL,"
						 + "  `defaultaddress` int NOT NULL,"
						 + "  `publickey` varchar(80) NOT NULL,"
						 + "  `track` int NOT NULL"
						 + ")";
		
		//Run it..
		stmt.execute(scriptsdb);
		
		//Create the base seed table..
		String seeddb = "CREATE TABLE IF NOT EXISTS `seed` ("
						 + "  `id` int NOT NULL UNIQUE,"
						 + "  `phrase` varchar(8192) NOT NULL,"
						 + "  `seed` varchar(80) NOT NULL"
						 + ")";

		//Run it..
		stmt.execute(seeddb);
		
		//All done..
		stmt.close();
		
		//KEY functions
		SQL_CREATE_PUBLIC_KEY 			= mSQLConnection.prepareStatement("INSERT IGNORE INTO keys ( size, depth, uses, maxuses, modifier, privatekey, publickey ) VALUES ( ?, ?, ?, ? ,? ,? ,? )");
		SQL_GET_ALL_KEYS				= mSQLConnection.prepareStatement("SELECT * FROM keys");
		SQL_GET_KEY						= mSQLConnection.prepareStatement("SELECT * FROM keys WHERE publickey=?");
		SQL_UPDATE_KEY_USES				= mSQLConnection.prepareStatement("UPDATE keys SET uses=? WHERE publickey=?");
		SQL_UPDATE_ALL_KEY_USES			= mSQLConnection.prepareStatement("UPDATE keys SET uses=?");
		SQL_UPDATE_INC_ALL_KEY_USES		= mSQLConnection.prepareStatement("UPDATE keys SET uses=uses+?");
		
		//Base Seed functions
		SQL_WIPE_PRIVATE_KEYS			= mSQLConnection.prepareStatement("UPDATE keys SET privatekey='0x00' WHERE privatekey!='0x00'");
		SQL_UPDATE_PRIVATE_KEYS			= mSQLConnection.prepareStatement("UPDATE keys SET privatekey=? WHERE publickey=?");
		
		//ScriptsDB
		SQL_ADD_SCRIPT				= mSQLConnection.prepareStatement("INSERT IGNORE INTO scripts ( script, address, simple, defaultaddress, publickey, track ) VALUES ( ? , ? , ? , ? , ? , ? )");
		SQL_REMOVE_SCRIPT			= mSQLConnection.prepareStatement("DELETE FROM scripts WHERE address=?");
		SQL_LIST_ALL_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts");
		SQL_LIST_SIMPLE_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE simple<>0");
		SQL_LIST_TRACK_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE track<>0");
		SQL_LIST_DEFAULT_SCRIPTS	= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE defaultaddress<>0");
		SQL_GET_SCRIPT				= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE address=?");
		
		//Seed DB
		SQL_SELECT_SEED				= mSQLConnection.prepareStatement("SELECT * FROM seed WHERE id=1");
		SQL_INSERT_SEED				= mSQLConnection.prepareStatement("INSERT INTO seed ( id, phrase, seed ) VALUES ( 1 , ? , ? )");				
		SQL_UPDATE_SEED				= mSQLConnection.prepareStatement("UPDATE seed SET phrase=?, seed=? WHERE id=1");				
		
		//Reset
		mAllKeys 			= new HashSet<>();
		mAllTrackedAddress 	= new HashSet<>();
		mAllSimpleAddress 	= new HashSet<>();
		
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
		
		//The seed phrase
		initBaseSeed();
		
		//Add the BASE MultiSig address - if not there
		String msaddress = new Address(multisig.MULTISIG_CONTRACT).getAddressData().to0xString();
		ScriptRow scr 	 = getScriptFromAddress(msaddress);
		if(scr == null) {
			MinimaLogger.log("Adding base MULTISIG address "+msaddress);
			addScript(multisig.MULTISIG_CONTRACT, false, false, "0x00", false);
		}
	}
	
	public void resetDB(String zNewSeedPhrase) throws SQLException {
		//One last statement
		Statement stmt = mSQLConnection.createStatement();
	
		//First wipe everything..
		stmt.execute("DROP ALL OBJECTS");
		
		//That's it..
		stmt.close();
		
		//Close the connection
		mSQLConnection.close();
		mSQLConnection = null;
		
		//Now reopen the Wallet..
		checkOpen();
		
		//And now reset the seed to the new phrase
		MiniData seed = BIP39.convertStringToSeed(zNewSeedPhrase);
		
		//Set it..
		updateSeedRow(zNewSeedPhrase, seed.to0xString());
	}
	
	private void initBaseSeed() throws SQLException {
	
		//Run the query
		ResultSet rs = SQL_SELECT_SEED.executeQuery();
		
		//Could be multiple results
		if(rs.next()) {
			
			//Store
			mBaseSeed = new SeedRow(rs);

			if(isBaseSeedAvailable()) {
				MinimaLogger.log("Base Private Seed Keys found");
			}else {
				MinimaLogger.log("Base Private Seed LOCKED");
			}
			
			return;
		}
		
		//Create a base row..
		MinimaLogger.log("Generating Base Private Seed Key");
		
		//Get a BIP39 phrase
		String[] words = BIP39.getNewWordList();
		
		//Convert to a string
		String phrase = BIP39.convertWordListToString(words);
		
		//Convert that into a seed..
		MiniData seed = BIP39.convertStringToSeed(phrase);
		
		//Now insert this..
		SQL_INSERT_SEED.clearParameters();
		SQL_INSERT_SEED.setString(1, phrase);
		SQL_INSERT_SEED.setString(2, seed.to0xString());
		
		//Run the query
		SQL_INSERT_SEED.execute();
		
		//And store..
		mBaseSeed = new SeedRow(phrase, seed.to0xString());
	}
	
	public void updateSeedRow(String zPhrase, String zSeed) throws SQLException {
		
		//Update
		SQL_UPDATE_SEED.clearParameters();
		SQL_UPDATE_SEED.setString(1, zPhrase);
		SQL_UPDATE_SEED.setString(2, zSeed);
		SQL_UPDATE_SEED.executeUpdate();
		
		//And Store..
		mBaseSeed = new SeedRow(zPhrase, zSeed);
	}
	
	public SeedRow getBaseSeed() {
		return mBaseSeed;
	}
	
	public boolean isBaseSeedAvailable() {
		return !mBaseSeed.getSeed().equals("0x00");
	}
	
	public void wipeBaseSeedRow() throws SQLException {
		//Wipe the DB
		SQL_WIPE_PRIVATE_KEYS.execute();
		
		//reset the base seed
		updateSeedRow("","0x00");
		
		//And Store..
		mBaseSeed = new SeedRow("", "0x00");
	}
	
	public boolean resetBaseSeedPrivKeys(String zPhrase, String zSeed) {
		
		try {
		
			//reset the base seed
			updateSeedRow(zPhrase, zSeed);
							
			//Get all the keys..
			ArrayList<KeyRow> keys = getAllKeys();
			
			//The seed
			MiniData seed = new MiniData(zSeed);
			
			//Now cycle through..
			for(KeyRow key : keys) {
				
				//Get the modifier..
				MiniData modifier = new MiniData(key.getModifier());
				
				//Now create a random private seed using the modifier
				MiniData privseed 	= Crypto.getInstance().hashObjects(seed, modifier);
				
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
	 * Check all the private keys are correct - BaseSeed  =modifier..
	 */
	public boolean checkAllPrivateKeys() {
		
		//Get all the keys..
		ArrayList<KeyRow> allkeys = getAllKeys();
		boolean allok = true;
		for(KeyRow kr : allkeys) {
			if(!checkSingleKey(kr.getPrivateKey(), kr.getModifier())) {
				MinimaLogger.log("[SERIOUS ERROR] Private key NOT == Seed+Modifier publickey:"+kr.getPublicKey());
				allok = false;
			}
		}
		
		return allok;
	}
	
	public boolean checkSingleKey(String zPrivateKey, String zModifier) {
		MiniData privatekey 	= new MiniData(zPrivateKey);
		MiniData mod 			= new MiniData(zModifier);
		MiniData privseed 		= Crypto.getInstance().hashObjects(new MiniData(mBaseSeed.getSeed()), mod);
		
		if(!privseed.isEqual(privatekey)) {
			return false;
		}
		
		return true;
	}
	
	/**
	 * Create an initial set of keys / addresses to use
	 */
	public int getDefaultKeysNumber() {
		return getAllDefaultAddresses().size();
	}
	
	public boolean initDefaultKeys(int zMaxNum) {
		return initDefaultKeys(zMaxNum, false);
	}
	
	public boolean initDefaultKeys(int zMaxNum, boolean zLog) {
		
		//Get all the keys..
		ArrayList<ScriptRow> allscripts = getAllDefaultAddresses();
		boolean allcreated = false;
		
		//Check we have the desired amount..
		int numkeys = allscripts.size();
		if(numkeys < NUMBER_GETADDRESS_KEYS) {
			
			//Create a few at a time..
			int diff = NUMBER_GETADDRESS_KEYS - numkeys;
			if(diff>zMaxNum) {
				diff = zMaxNum;
			}
			
			//Create the keys
			for(int i=0;i<diff;i++) {
				if(!isShuttingDown()) {
					createNewSimpleAddress(true);
					
					if(zLog) {
						MinimaLogger.log("Default key created.. total:"+(numkeys+i+1));
					}
				}
			}
			
			MinimaLogger.log(diff+" more initial keys created.. Total now : "+(numkeys+diff)+" / "+NUMBER_GETADDRESS_KEYS);
		}else {
			allcreated = true;
		}
		
		if(getAllDefaultAddresses().size() >= NUMBER_GETADDRESS_KEYS) {
			return true;
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
			throw new IllegalArgumentException("KeysDB LOCKED. No More Keys Allowed. Base SEED missing..");
		}
		
		//Create a random modifier..
		int numkeys 		= mAllKeys.size();
		MiniData modifier 	= new MiniData(new BigInteger(Integer.toString(numkeys)));

		//Now create a random private seed using the modifier
		MiniData privseed 	= Crypto.getInstance().hashObjects(new MiniData(mBaseSeed.getSeed()), modifier);
		
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
	 * Remove a script
	 */
	public synchronized void removeScript(String zAddress) {
		//Now put all this in the DB
		try {
		
			//Get the Query ready
			SQL_REMOVE_SCRIPT.clearParameters();
		
			//Set main params
			SQL_REMOVE_SCRIPT.setString(1, zAddress);
			
			//Do it.
			SQL_REMOVE_SCRIPT.execute();
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
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
		return mAllKeys.contains(zPublicKey);
	}
	
	/**
	 * Is this ADDRESS relevant to us - are we tracking it
	 */
	public synchronized boolean isAddressRelevant(String zAddress) {
		return mAllTrackedAddress.contains(zAddress);
	}
	
	/**
	 * Is this ADDRESS a simple address
	 */
	public synchronized boolean isAddressSimple(String zAddress) {
		return mAllSimpleAddress.contains(zAddress);
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
			
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
		
		return allscripts;
	}
	
	/**
	 * Sign a piece of data with a specific public key
	 */
	public Signature signData(String zPublicKey, MiniData zData) {
		
		//Check we can create new keys
		if(!isBaseSeedAvailable()) {
			throw new IllegalArgumentException("KeysDB LOCKED. No Private Keys..");
		}
		
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
	
	/**
	 * After an Archive resync..
	 */
	public void updateAllKeyUses(int zUses) throws SQLException {		

		//Get the Query ready
		SQL_UPDATE_ALL_KEY_USES.clearParameters();
	
		//Set main params
		SQL_UPDATE_ALL_KEY_USES.setInt(1, zUses);
		
		//Run the query
		SQL_UPDATE_ALL_KEY_USES.execute();
	}
	
	/**
	 * After a Restore
	 */
	public void updateIncrementAllKeyUses(int zIncrementUses) throws SQLException {		

		//Get the Query ready
		SQL_UPDATE_INC_ALL_KEY_USES.clearParameters();
	
		//Set main params
		SQL_UPDATE_INC_ALL_KEY_USES.setInt(1, zIncrementUses);
		
		//Run the query
		SQL_UPDATE_INC_ALL_KEY_USES.execute();
	}
}
