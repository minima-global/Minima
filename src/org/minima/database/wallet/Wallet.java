package org.minima.database.wallet;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Random;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
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
	private MiniData mMainPrivateSeed = new MiniData("0x000102"); 
	
	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_CREATE_PUBLIC_KEY 			= null;
	PreparedStatement SQL_GET_KEY 						= null;
	PreparedStatement SQL_GET_ALL_KEYS 					= null;
	
	PreparedStatement SQL_GET_USES 						= null;
	PreparedStatement SQL_UPDATE_USES 					= null;
	
//	PreparedStatement SQL_GET_ALL_NONSINGLE_RELEVANT 	= null;
	PreparedStatement SQL_GET_PUBKEY_FADDRESS 			= null;
//	PreparedStatement SQL_GET_FPUBLICKEY 				= null;
	
	
	/**
	 * Scripts DB
	 */
	PreparedStatement SQL_ADD_SCRIPT 			= null;
	PreparedStatement SQL_LIST_ALL_SCRIPTS 		= null;
	PreparedStatement SQL_LIST_SIMPLE_SCRIPTS 	= null;
	PreparedStatement SQL_LIST_TRACK_SCRIPTS 	= null;
	PreparedStatement SQL_GET_SCRIPT 			= null;
	
	/**
	 * Stop creating keys if you are
	 */
	boolean mShuttingdown = false;
	
	public Wallet() {
		super();
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
			
			SQL_UPDATE_USES					= mSQLConnection.prepareStatement("UPDATE keys SET uses=? WHERE privatekey=?");
			SQL_GET_USES					= mSQLConnection.prepareStatement("SELECT uses FROM keys WHERE privatekey=?");
		
			SQL_GET_PUBKEY_FADDRESS			= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE simple=1 AND address=?");
//			SQL_GET_FPUBLICKEY				= mSQLCOnnection.prepareStatement("SELECT * FROM keys WHERE publickey=?");
			
			//ScriptsDB
			SQL_ADD_SCRIPT				= mSQLConnection.prepareStatement("INSERT IGNORE INTO scripts ( script, address, simple, publickey, track ) VALUES ( ? , ? , ? , ? , ? )");
			SQL_LIST_ALL_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts");
			SQL_LIST_SIMPLE_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE simple<>0");
			SQL_LIST_TRACK_SCRIPTS		= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE track<>0");
			SQL_GET_SCRIPT				= mSQLConnection.prepareStatement("SELECT * FROM scripts WHERE address=?");
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
	}
	
	/**
	 * Create an initial set of keys / addresses to use
	 */
	public boolean initDefaultKeys() {
		
		//Get all the keys..
		ArrayList<ScriptRow> allscripts = getAllSimpleAddresses();
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
					createNewSimpleAddress();
				}
			}
			
			MinimaLogger.log("8 more initial keys created.. Total now : "+(numkeys+diff));
		}else {
			allcreated = true;
		}
		
		return allcreated;
	}
	
	/**
	 * Get 1 of your default addresses
	 */
	public ScriptRow getDefaultKeyAddress() {
		//Get all the keys..
		ArrayList<ScriptRow> allkeys = getAllSimpleAddresses();
		int numkeys = allkeys.size();
		
		//Now pick a random key..
		int rand = new Random().nextInt(numkeys);
		
		return allkeys.get(rand);
	}
	
	/**
	 * Create a NEW Simple Address
	 */
	public synchronized ScriptRow createNewSimpleAddress() {
				
		//Create a NEW random seed..
		MiniData privateseed = MiniData.getRandomData(32);
		
		//Now create a new KEY
		KeyRow key = createNewKey(privateseed);
		
		//Now create a simple address..
		String script = new String("RETURN SIGNEDBY("+key.getPublicKey()+")");
		
		//Now add to the database..
		return addScript(script, true, key.getPublicKey(), true);
	}
	
	/**
	 * Create a NEW key
	 */
	public synchronized KeyRow createNewKey(MiniData zPrivateSeed) {
		
		//Make the TreeKey
		TreeKey treekey 	= TreeKey.createDefault(zPrivateSeed);
		
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
			SQL_CREATE_PUBLIC_KEY.setString(5, "0x00");
			
			SQL_CREATE_PUBLIC_KEY.setString(6, treekey.getPrivateKey().to0xString());
			SQL_CREATE_PUBLIC_KEY.setString(7, treekey.getPublicKey().to0xString());
			
			//Do it.
			SQL_CREATE_PUBLIC_KEY.execute();
			
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
	public synchronized ScriptRow addScript(String zScript, boolean zSimple, String zPublicKey, boolean zTrack) {
		
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
			
			SQL_ADD_SCRIPT.setString(4, zPublicKey);
			
			if(zTrack) {
				SQL_ADD_SCRIPT.setInt(5, 1);
			}else {
				SQL_ADD_SCRIPT.setInt(5, 0);
			}
			
			//Do it.
			SQL_ADD_SCRIPT.execute();
			
			return new ScriptRow(zScript, addr, zSimple, zPublicKey, zTrack);
			
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
		return true;
	}
	
	/**
	 * Is this ADDRESS relevant to us - are we tracking it
	 */
	public synchronized boolean isAddressRelevant(String zAddress) {
		return true;
	}
	
	/**
	 * Is this ADDRESS a simple address
	 */
	public synchronized boolean isAddressSimple(String zAddress) {
		return true;
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
	
	private synchronized ArrayList<ScriptRow> getAllTrackedAddresses() {
		
		//Do both sets..
		ArrayList<ScriptRow> allscripts = new ArrayList<>();
		try {
			
			//Run the query
			ResultSet rs = SQL_LIST_TRACK_SCRIPTS.executeQuery();
			
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

	public synchronized ArrayList<ScriptRow> getAllSimpleAddresses() {
		
		//Do both sets..
		ArrayList<ScriptRow> allscripts = new ArrayList<>();
		try {
			
			//Run the query
			ResultSet rs = SQL_LIST_SIMPLE_SCRIPTS.executeQuery();
			
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
	
//	/**
//	 * Get all relevant Public Keys and Addresses
//	 */
//	public synchronized ArrayList<KeyRow> getAllRelevant(boolean zOnlyNonSingleKeys) {
//		
//		//If nop change use the cached version
//		if(!mKeyRowChange) {
//			if(zOnlyNonSingleKeys) {
//				return mCachedRelevantNonSingleKeys;
//			}
//			
//			return mCachedRelevantAllKeys;
//		}
//		
//		//And now get all the custom scripts..
//		ArrayList<KeyRow> customscripts = getAllCustomScripts();
//		
//		//Do both sets..
//		ArrayList<KeyRow> allkeys = new ArrayList<>();
//		try {
//			
//			//Run the query
//			ResultSet rs = SQL_GET_ALL_NONSINGLE_RELEVANT.executeQuery();
//			
//			//Could be multiple results
//			while(rs.next()) {
//				
//				//Get the details
//				String publickey 	= rs.getString("publickey");
//				String address 	 	= rs.getString("simpleaddress");
//				String script 	 	= rs.getString("script");
//				String privatekey 	= rs.getString("privatekey");
//				
//				//Add to our list
//				allkeys.add(new KeyRow(privatekey, publickey, address, script, true));
//			}
//			
//		} catch (SQLException e) {
//			MinimaLogger.log(e);
//		}
//		
//		//Non single keys
//		mCachedRelevantNonSingleKeys = allkeys;
//		mCachedRelevantNonSingleKeys.addAll(customscripts);
//		
//		//Now all the keys..
//		allkeys = new ArrayList<>();
//		try {
//			
//			//Run the query
//			ResultSet rs = SQL_GET_ALL_KEYS.executeQuery();
//			
//			//Could be multiple results
//			while(rs.next()) {
//				
//				//Get the details
//				String publickey 	= rs.getString("publickey");
//				String address 	 	= rs.getString("simpleaddress");
//				String script 	 	= rs.getString("script");
//				String privatekey 	= rs.getString("privatekey");
//				
//				//Add to our list
//				allkeys.add(new KeyRow(privatekey, publickey, address, script, true));
//			}
//			
//		} catch (SQLException e) {
//			MinimaLogger.log(e);
//		}
//		
//		//All keys
//		mCachedRelevantAllKeys = allkeys;
//		mCachedRelevantAllKeys.addAll(customscripts);
//			
//		//Ok - no key change for now..
//		mKeyRowChange		= false;
//		
//		//What to return
//		if(zOnlyNonSingleKeys) {
//			return mCachedRelevantNonSingleKeys;
//		}
//		
//		return mCachedRelevantAllKeys;
//	}
	
	/**
	 * Get the public key row for this address - IF it's simple
	 */
//	public synchronized String getPublicKeyFromAddress(String zAddres) {
//		try {
//			
//			//Get the Query ready
//			SQL_GET_PUBKEY_FADDRESS.clearParameters();
//		
//			//Set main params
//			SQL_GET_PUBKEY_FADDRESS.setString(1, zAddres);
//			
//			//Run the query
//			ResultSet rs = SQL_GET_PUBKEY_FADDRESS.executeQuery();
//			
//			//Could be multiple results
//			if(rs.next()) {
//				
//				//Get the publickey
//				String pubkey = rs.getString("publickey");
//				
//				return pubkey;
//			}
//			
//		} catch (SQLException e) {
//			MinimaLogger.log(e);
//		}
//		
//		return null;
//	}
	
//	/**
//	 * Get the full row for this address
//	 */
//	public synchronized KeyRow getKeysRowFromPublicKey(String zPubk) {
//		try {
//			
//			//Get the Query ready
//			SQL_GET_FPUBLICKEY.clearParameters();
//		
//			//Set main params
//			SQL_GET_FPUBLICKEY.setString(1, zPubk);
//			
//			//Run the query
//			ResultSet rs = SQL_GET_FPUBLICKEY.executeQuery();
//			
//			//Could be multiple results
//			if(rs.next()) {
//				
//				//Get the txpowid
//				String privkey 	= rs.getString("privatekey");
//				String pubkey 	= rs.getString("publickey");
//				String script 	= rs.getString("script");
//				String address 	= rs.getString("simpleaddress");
//				
//				return new KeyRow(privkey, pubkey, address, script, true);
//			}
//			
//		} catch (SQLException e) {
//			MinimaLogger.log(e);
//		}
//		
//		return null;
//	}
	
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
			TreeKey tk = TreeKey.createDefault(new MiniData(key.getPrivateKey()));
			
			//How many times has this been used.. get from DB
			int uses = key.getUses();
			
			//Set this..
			tk.setUses(uses);
			
			//Now we have the Key..
			Signature signature = tk.sign(zData);
			
			//Update the DB..
			uses = tk.getUses();
			
			//Update the DB..
			updateUses(key.getPrivateKey(), uses);
			
			//And finally return the sig..
			return signature;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	/*private synchronized int getUses(String zPrivateKey) throws SQLException {		
		//Get the Query ready
		SQL_GET_USES.clearParameters();
	
		//Set main params
		SQL_GET_USES.setString(1, zPrivateKey);
		
		//Run the query
		ResultSet rs = SQL_GET_USES.executeQuery();
		
		//Could be multiple results
		if(rs.next()) {
			
			//Get the uses
			int uses = rs.getInt("uses");
			
			return uses;
		}
		
		return 0;
	}*/
	
	private void updateUses(String zPrivateKey, int zUses) throws SQLException {		

		//Get the Query ready
		SQL_UPDATE_USES.clearParameters();
	
		//Set main params
		SQL_UPDATE_USES.setInt(1, zUses);
		SQL_UPDATE_USES.setString(2, zPrivateKey);
		
		//Run the query
		SQL_UPDATE_USES.execute();
	}
}
