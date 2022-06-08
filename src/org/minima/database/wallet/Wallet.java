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
	 * PreparedStatements
	 */
	PreparedStatement SQL_CREATE_PUBLIC_KEY 		= null;
	PreparedStatement SQL_GET_ALL_RELEVANT 			= null;
	PreparedStatement SQL_GET_FADDRESS 				= null;
	PreparedStatement SQL_GET_FPUBLICKEY 			= null;
	PreparedStatement SQL_GET_USES 					= null;
	PreparedStatement SQL_UPDATE_USES 				= null;
	
	/**
	 * Scripts DB
	 */
	PreparedStatement SQL_ADD_CUSTOM_SCRIPT 		= null;
	PreparedStatement SQL_LIST_CUSTOM_SCRIPTS 		= null;
	
	/**
	 * A list of previously used TreeKeys.. no need to reinit them if we have them allready
	 */
	Hashtable<String, TreeKey> mTreeKeys = new Hashtable<>();
	
	/**
	 * Has there been a change to the Key Rows.. otherwise used cached
	 */
	boolean mKeyRowChange 					= true;
	ArrayList<KeyRow> mCachedRelevantKeys 	= new ArrayList<>();
	
	public Wallet() {
		super();
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLCOnnection.createStatement();
			
			//Create main table
			String create =   "CREATE TABLE IF NOT EXISTS `keys` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `basemodifier` bigint NOT NULL,"
							+ "  `uses` bigint NOT NULL,"
							+ "  `privatekey` varchar(80) NOT NULL,"
							+ "  `publickey` varchar(80) NOT NULL,"
							+ "  `script` varchar(255) NOT NULL,"
							+ "  `simpleaddress` varchar(80) NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(create);
			
			//Create scripts table
			String scriptsdb = "CREATE TABLE IF NOT EXISTS `scripts` ("
							 + "  `id` IDENTITY PRIMARY KEY,"
							 + "  `script` varchar(8192) NOT NULL,"
							 + "  `address` varchar(80) NOT NULL,"
							 + "  `track` int NOT NULL"
							 + ")";
			
			//Run it..
			stmt.execute(scriptsdb);
			
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			SQL_CREATE_PUBLIC_KEY 	= mSQLCOnnection.prepareStatement("INSERT IGNORE INTO keys ( basemodifier, uses, privatekey, publickey, script, simpleaddress ) VALUES ( ?, ?, ? ,? ,? ,? )");
			SQL_GET_ALL_RELEVANT	= mSQLCOnnection.prepareStatement("SELECT * FROM keys");
			SQL_GET_FADDRESS		= mSQLCOnnection.prepareStatement("SELECT * FROM keys WHERE simpleaddress=?");
			SQL_GET_FPUBLICKEY		= mSQLCOnnection.prepareStatement("SELECT * FROM keys WHERE publickey=?");
			SQL_UPDATE_USES			= mSQLCOnnection.prepareStatement("UPDATE keys SET uses=? WHERE privatekey=?");
			SQL_GET_USES			= mSQLCOnnection.prepareStatement("SELECT uses FROM keys WHERE privatekey=?");
			
			//ScriptsDB
			SQL_ADD_CUSTOM_SCRIPT	= mSQLCOnnection.prepareStatement("INSERT IGNORE INTO scripts ( script, address, track ) VALUES ( ?, ? , ? )");
			SQL_LIST_CUSTOM_SCRIPTS	= mSQLCOnnection.prepareStatement("SELECT * FROM scripts");
			
			mKeyRowChange = true;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
	}
	
	/**
	 * Create an initial set of keys / addresses to use
	 */
	public void initDefaultKeys() {
		
		//Get all the keys..
		ArrayList<KeyRow> allkeys = getAllRelevant();
		
		//Check we have the desired amount..
		int numkeys = allkeys.size();
		if(numkeys < 16) {
			
			MinimaLogger.log("Creating initial key set..");
			
			//Create the remaining keys
			int create = 16 - numkeys;
			for(int i=0;i<create;i++) {
				createNewKey();
			}
		}
	}
	
	/**
	 * Get 1 of your keys at random
	 */
	public KeyRow getKey() {
		//Get all the keys..
		ArrayList<KeyRow> allkeys = getAllRelevant();
		int numkeys = allkeys.size();
		
		//Now pick a random key..
		int rand = new Random().nextInt(numkeys);
		
		return allkeys.get(rand);
	}
	
	/**
	 * Create a NEW key
	 */
	public synchronized KeyRow createNewKey() {
				
		//Create a NEW random seed..
		MiniData privateseed = MiniData.getRandomData(32);
		
		return createNewKey(privateseed);
	}
	
	/**
	 * Create a NEW key
	 */
	public synchronized KeyRow createNewKey(MiniData zPrivateSeed) {
		
		//Change has occurred
		mKeyRowChange = true;
		
		//Make the TreeKey
		TreeKey treekey 	= TreeKey.createDefault(zPrivateSeed);
		String privstring 	= zPrivateSeed.to0xString();
		
		//Add to our list..
		mTreeKeys.put(privstring, treekey);
		
		//Get the public key
		MiniData pubkey = treekey.getPublicKey();
		
		//The script
		String script = new String("RETURN SIGNEDBY("+pubkey.to0xString()+")");
		
		//And create the simple spend
		Address addr  	= new Address(script);
	
		//Now put all this in the DB
		try {
			
			//Get the Query ready
			SQL_CREATE_PUBLIC_KEY.clearParameters();
		
			//Set main params
			SQL_CREATE_PUBLIC_KEY.setInt(1, 0); // FOR NOW.. not based off seed phrase
			SQL_CREATE_PUBLIC_KEY.setInt(2, 0);
			SQL_CREATE_PUBLIC_KEY.setString(3, privstring);
			SQL_CREATE_PUBLIC_KEY.setString(4, pubkey.to0xString());
			SQL_CREATE_PUBLIC_KEY.setString(5, script);
			SQL_CREATE_PUBLIC_KEY.setString(6, addr.getAddressData().to0xString());
			
			//Do it.
			SQL_CREATE_PUBLIC_KEY.execute();
			
			return new KeyRow(privstring, pubkey.to0xString(), addr.getAddressData().to0xString(), script, true);
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	/**
	 * Get all relevant Public Keys and Addresses
	 */
	public synchronized ArrayList<KeyRow> getAllRelevant() {
		
		//If nop change use the cached version
		if(!mKeyRowChange) {
			return mCachedRelevantKeys;
		}
		
		ArrayList<KeyRow> allkeys = new ArrayList<>();
		
		try {
			
			//Run the query
			ResultSet rs = SQL_GET_ALL_RELEVANT.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the details
				String publickey 	= rs.getString("publickey");
				String address 	 	= rs.getString("simpleaddress");
				String script 	 	= rs.getString("script");
				String privatekey 	= rs.getString("privatekey");
				
				//Add to our list
				allkeys.add(new KeyRow(privatekey, publickey, address, script, true));
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		//And now get all the custom scripts..
		ArrayList<KeyRow> customscripts = getAllCustomScripts();
		allkeys.addAll(customscripts);
		
		//Store for later
		mCachedRelevantKeys = allkeys;
		mKeyRowChange		= false;
		
		return allkeys;
	}
	
	/**
	 * Get the full row for this address
	 */
	public synchronized KeyRow getKeysRowFromAddress(String zAddres) {
		try {
			
			//Get the Query ready
			SQL_GET_FADDRESS.clearParameters();
		
			//Set main params
			SQL_GET_FADDRESS.setString(1, zAddres);
			
			//Run the query
			ResultSet rs = SQL_GET_FADDRESS.executeQuery();
			
			//Could be multiple results
			if(rs.next()) {
				
				//Get the txpowid
				String privkey 	= rs.getString("privatekey");
				String pubkey 	= rs.getString("publickey");
				String script 	= rs.getString("script");
				String address 	= rs.getString("simpleaddress");
				
				return new KeyRow(privkey, pubkey, address, script, true);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		//Could be a custom Script..
		ArrayList<KeyRow> customscripts = getAllCustomScripts();
		for(KeyRow kr : customscripts) {
			if(kr.getAddress().equals(zAddres)) {
				return kr;
			}
		}
		
		return null;
	}
	
	/**
	 * Get the full row for this address
	 */
	public synchronized KeyRow getKeysRowFromPublicKey(String zPubk) {
		try {
			
			//Get the Query ready
			SQL_GET_FPUBLICKEY.clearParameters();
		
			//Set main params
			SQL_GET_FPUBLICKEY.setString(1, zPubk);
			
			//Run the query
			ResultSet rs = SQL_GET_FPUBLICKEY.executeQuery();
			
			//Could be multiple results
			if(rs.next()) {
				
				//Get the txpowid
				String privkey 	= rs.getString("privatekey");
				String pubkey 	= rs.getString("publickey");
				String script 	= rs.getString("script");
				String address 	= rs.getString("simpleaddress");
				
				return new KeyRow(privkey, pubkey, address, script, true);
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	/**
	 * Sign a piece of data with a specific public key
	 */
	public Signature sign(String zPrivate, MiniData zData) {
		
		try {
			
			//First get the TreeKey.. we may have already used it.. 
			if(!mTreeKeys.containsKey(zPrivate)) {
				
				//Create a new TreeKey
				TreeKey tknew = TreeKey.createDefault(new MiniData(zPrivate));
				
				//How many times has this been used.. get from DB
				int uses = getUses(zPrivate);
				
				//Set this..
				tknew.setUses(uses);
				
				//Add it to our list..
				mTreeKeys.put(zPrivate, tknew);
			}
			
			//Get it..
			TreeKey tk = mTreeKeys.get(zPrivate); 
			
			//Now we have the Key..
			Signature signature = tk.sign(zData);
			
			//Update the DB..
			int uses = tk.getUses();
			
			//Update the DB..
			updateUses(zPrivate, uses);
			
			//And finally return the sig..
			return signature;
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	private synchronized int getUses(String zPrivateKey) throws SQLException {		
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
	}
	
	private synchronized void updateUses(String zPrivateKey, int zUses) throws SQLException {		
		//Change has occurred
		mKeyRowChange = true;
				
		//Get the Query ready
		SQL_UPDATE_USES.clearParameters();
	
		//Set main params
		SQL_UPDATE_USES.setInt(1, zUses);
		SQL_UPDATE_USES.setString(2, zPrivateKey);
		
		//Run the query
		SQL_UPDATE_USES.execute();
	}
	
	/**
	 * Add a custom script
	 */
	public synchronized KeyRow addScript(String zScript, boolean zTrack) {
		
		//Change has occurred
		mKeyRowChange = true;
		
		//And create the simple spend
		Address addr  	= new Address(zScript);
		
		//Now put all this in the DB
		try {
			
			//Get the Query ready
			SQL_ADD_CUSTOM_SCRIPT.clearParameters();
		
			//Set main params
			SQL_ADD_CUSTOM_SCRIPT.setString(1, zScript);
			SQL_ADD_CUSTOM_SCRIPT.setString(2, addr.getAddressData().to0xString());
			
			//Do we track ALL these addresses..
			if(zTrack) {
				SQL_ADD_CUSTOM_SCRIPT.setInt(3, 1);
			}else {
				SQL_ADD_CUSTOM_SCRIPT.setInt(3, 0);
			}
			
			//Do it.
			SQL_ADD_CUSTOM_SCRIPT.execute();
			
			return new KeyRow("", "", addr.getAddressData().to0xString(), zScript, zTrack);
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	/**
	 * Get all your custom scripts
	 */
	public synchronized ArrayList<KeyRow> getAllCustomScripts() {
		
		ArrayList<KeyRow> allkeys = new ArrayList<>();
		
		try {
			
			//Run the query
			ResultSet rs = SQL_LIST_CUSTOM_SCRIPTS.executeQuery();
			
			//Could be multiple results
			while(rs.next()) {
				
				//Get the details
				String address 	 = rs.getString("address");
				String script 	 = rs.getString("script");
				int track		 = rs.getInt("track");
				
				//Create the KeyRow
				if(track == 0) {
					allkeys.add(new KeyRow("", "", address, script, false));
				}else {
					allkeys.add(new KeyRow("", "", address, script, true));
				}
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		return allkeys;
	}
}
