package org.minima.database.userdb;

import java.util.ArrayList;

import org.minima.database.userdb.java.reltxpow;
import org.minima.objects.Address;
import org.minima.objects.PubPrivKey;
import org.minima.objects.TokenDetails;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;

public interface UserDB {

	/**
	 * Get all the public private Keys 
	 */
	public ArrayList<PubPrivKey> getKeys();
	public PubPrivKey newPublicKey();
	public PubPrivKey getPubPrivKey(MiniData zPubKey);
	
	/**
	 * Get all the addresses
	 */
	public ArrayList<Address> getSimpleAddresses();
	public Address newSimpleAddress();
	public Address newSimpleAddress(PubPrivKey zPubPriv);
	public boolean isSimpleAddress(MiniHash zAddress);
	public MiniData getPublicKey(MiniHash zAddress);
	
	public ArrayList<Address> getScriptAddresses();
	public Address newScriptAddress(String zScript);
	public ArrayList<Address> getAllAddresses();
	public String getScript(MiniHash zAddress);
	
	/**
	 * Check if an address matters
	 */
	public boolean isAddressRelevant(MiniHash zAddress);
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
	public ArrayList<TokenDetails> getAllKnownTokens();
	
	public TokenDetails getTokenDetail(MiniHash zTokenID);
	
	public void addTokenDetails(TokenDetails zToken);
	
	/**
	 * Transaction History
	 */
	public ArrayList<reltxpow> getHistory();
	public void addToHistory(TxPOW zTxPOW, MiniNumber zValue);
//	public void removeHistory(MiniHash zTxPowID);
	public void clearHistory();
	
}
