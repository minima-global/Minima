package org.minima.database.userdb;

import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.database.userdb.java.reltxpow;
import org.minima.objects.Address;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
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
	
	/**
	 * Create a new Simple Address
	 */
	public Address newSimpleAddress();
	public Address newSimpleAddress(int zBitLength);
	public Address newSimpleAddress(PubPrivKey zPubPriv);
	
	public boolean isSimpleAddress(MiniData zAddress);
	public MiniData getPublicKeyForSimpleAddress(MiniData zAddress);
	
	/**
	 * Create a new Custom Address JUST for you
	 */
	public Address newScriptAddress(String zScript);
	
	/**
	 * Add an EXTRA Script - will not mean you store all outputs of this type
	 * 
	 * Ouptuts are stored if you own a KEY in the STATE
	 */
	public Address newExtraAddress(String zScript);
	
	/**
	 * get all KNOWN addresses
	 */
	public ArrayList<Address> getAllAddresses();
	
	/**
	 * Get the Script for a known address
	 */
	public String getScript(MiniData zAddress);
	
	/**
	 * Check if an address matters
	 */
	public boolean isAddressRelevant(MiniData zAddress);
	
	/**
	 * Check if a transaction matters..
	 * @param zTrans
	 * @return
	 */
	public boolean isTransactionRelevant(Transaction zTrans);
	
	/**
	 * Check if the state of this transaction has a KEY we own.. 
	 * ALL unspent outputs are then tracked - they could be for you
	 * @param zTrans
	 * @return
	 */
	public boolean isStateListRelevant(ArrayList<StateVariable> zStateVarList);
	
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
	public void addToHistory(TxPOW zTxPOW, Hashtable<String, MiniNumber> zValues);
	public void clearHistory();
	
}
