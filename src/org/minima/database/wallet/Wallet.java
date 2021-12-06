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
	PreparedStatement SQL_GET_SCRIPT 				= null;
	PreparedStatement SQL_GET_USES 					= null;
	PreparedStatement SQL_UPDATE_USES 				= null;
	
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
			String create = "CREATE TABLE IF NOT EXISTS `keys` ("
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
					
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			SQL_CREATE_PUBLIC_KEY 	= mSQLCOnnection.prepareStatement("INSERT IGNORE INTO keys ( basemodifier, uses, privatekey, publickey, script, simpleaddress ) VALUES ( ?, ?, ? ,? ,? ,? )");
			SQL_GET_ALL_RELEVANT	= mSQLCOnnection.prepareStatement("SELECT * FROM keys");
			SQL_GET_SCRIPT			= mSQLCOnnection.prepareStatement("SELECT * FROM keys WHERE simpleaddress=?");
			
			SQL_UPDATE_USES			= mSQLCOnnection.prepareStatement("UPDATE keys SET uses=? WHERE privatekey=?");
			SQL_GET_USES			= mSQLCOnnection.prepareStatement("SELECT uses FROM keys WHERE privatekey=?");
			
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
		
		//Change has occurred
		mKeyRowChange = true;
		
		//Create a NEW random seed..
		MiniData privateseed = MiniData.getRandomData(32);
		
		//Make the TreeKey
		TreeKey treekey = TreeKey.createDefault(privateseed);
		
		//Add to our list..
		mTreeKeys.put(privateseed.to0xString(), treekey);
		
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
			SQL_CREATE_PUBLIC_KEY.setString(3, privateseed.to0xString());
			SQL_CREATE_PUBLIC_KEY.setString(4, pubkey.to0xString());
			SQL_CREATE_PUBLIC_KEY.setString(5, script);
			SQL_CREATE_PUBLIC_KEY.setString(6, addr.getAddressData().to0xString());
			
			//Do it.
			SQL_CREATE_PUBLIC_KEY.execute();
			
			return new KeyRow(privateseed.to0xString(), pubkey.to0xString(), addr.getAddressData().to0xString(), script);
			
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
				String publickey = rs.getString("publickey");
				String address 	 = rs.getString("simpleaddress");
				String script 	 = rs.getString("script");
				
				//Add to our list
				allkeys.add(new KeyRow("",publickey, address, script));
			}
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
		
		//Store for later
		mCachedRelevantKeys = allkeys;
		mKeyRowChange		= false;
		
		return allkeys;
	}
	
	/**
	 * Get the full row for this address
	 */
	public synchronized KeyRow getKeysRow(String zAddres) {
		try {
			
			//Get the Query ready
			SQL_GET_SCRIPT.clearParameters();
		
			//Set main params
			SQL_GET_SCRIPT.setString(1, zAddres);
			
			//Run the query
			ResultSet rs = SQL_GET_SCRIPT.executeQuery();
			
			//Could be multiple results
			if(rs.next()) {
				
				//Get the txpowid
				String privkey 	= rs.getString("privatekey");
				String pubkey 	= rs.getString("publickey");
				String script 	= rs.getString("script");
				String address 	= rs.getString("simpleaddress");
				
				return new KeyRow(privkey, pubkey, address, script);
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
}
