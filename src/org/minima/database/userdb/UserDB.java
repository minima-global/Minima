package org.minima.database.userdb;

import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.database.userdb.java.reltxpow;
import org.minima.objects.Address;
import org.minima.objects.PubPrivKey;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;

public interface UserDB {

	/**
	 * Get all the public private Keys 
	 */
	public ArrayList<PubPrivKey> getKeys();
	public PubPrivKey newPublicKey(int zBitLength);
	public PubPrivKey getPubPrivKey(MiniData zPubKey);
	
	/**
	 * Get all the addresses
	 */
	public ArrayList<Address> getSimpleAddresses();
	public Address newSimpleAddress();
	public Address newSimpleAddress(int zBitLength);
	public Address newSimpleAddress(PubPrivKey zPubPriv);
	public boolean isSimpleAddress(MiniData zAddress);
	public MiniData getPublicKey(MiniData zAddress);
	
	public ArrayList<Address> getScriptAddresses();
	public Address newScriptAddress(String zScript);
	public ArrayList<Address> getAllAddresses();
	public String getScript(MiniData zAddress);
	
	/**
	 * Check if an address matters
	 */
	public boolean isAddressRelevant(MiniData zAddress);
	public boolean isTransactionRelevant(Transaction zTrans);
	
	/**
	 * Custom Transactions
	 */
	public ArrayList<UserDBRow> getAllRows();
	
	public UserDBRow getUserRow(int zID);
	
	public UserDBRow addUserRow(int zID);
	
	public void deleteUserRow(int zID);
	
	/**
	 * Token Details
	 */
	public ArrayList<TokenProof> getAllKnownTokens();
	
	public TokenProof getTokenDetail(MiniData zTokenID);
	
	public void addTokenDetails(TokenProof zToken);
	
	/**
	 * Transaction History
	 */
	public ArrayList<reltxpow> getHistory();
	public void addToHistory(MiniData zTxPOWID, Hashtable<String, MiniNumber> zValues);
	public void clearHistory();
	
}
