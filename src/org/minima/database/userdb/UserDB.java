package org.minima.database.userdb;

import java.util.ArrayList;

import org.minima.objects.Address;
import org.minima.objects.PubPrivKey;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;

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
	public boolean isSimpleAddress(MiniData32 zAddress);
	public MiniData getPublicKey(MiniData32 zAddress);
	
	public ArrayList<Address> getScriptAddresses();
	public Address newScriptAddress(String zScript);
	public ArrayList<Address> getAllAddresses();
	public String getScript(MiniData32 zAddress);
	
	/**
	 * Check if an address matters
	 */
	public boolean isAddressRelevant(MiniData32 zAddress);
	public boolean isTransactionRelevant(Transaction zTrans);
	
	/**
	 * Custom Transactions
	 */
	public ArrayList<UserDBRow> getAllRows();
	
	public UserDBRow getUserRow(int zID);
	
	public UserDBRow addUserRow();

	public void deleteUserRow(int zID);
}
