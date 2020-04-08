package org.minima.database.userdb.java;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.UserDBRow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;
import org.minima.utils.Streamable;

public class JavaUserDB implements UserDB, Streamable{
	
	ArrayList<PubPrivKey> mPubPrivKeys;
	ArrayList<Address>    mAddresses;
	ArrayList<Address>    mScriptAddresses;
	
	int mCounter;
	ArrayList<UserDBRow> mRows;
	
	ArrayList<Address> mTotalAddresses;
	
	/**
	 * Token Details
	 */
	ArrayList<TokenProof> mAllTokens;
	
	/**
	 * Transaction History
	 */
	ArrayList<reltxpow> mHistory;
	
	/**
	 * Base constructor
	 */
	public JavaUserDB() {
		mPubPrivKeys 	 = new ArrayList<>();
		mAddresses 		 = new ArrayList<>();
		mScriptAddresses = new ArrayList<>();
		mTotalAddresses  = new ArrayList<>();
		mAllTokens		 = new ArrayList<>();
		
		mCounter = 0;
		mRows  = new ArrayList<>();
		
		mHistory = new ArrayList<>();
	}
	
	@Override
	public ArrayList<Address> getAllAddresses(){
		return mTotalAddresses;
	}

	@Override
	public ArrayList<PubPrivKey> getKeys() {
		return mPubPrivKeys;
	}

	@Override
	public PubPrivKey newPublicKey(int zBitLength) {
		PubPrivKey pubkey = new PubPrivKey(zBitLength);
		mPubPrivKeys.add(pubkey);
		return pubkey;
	}
	
	@Override
	public ArrayList<UserDBRow> getAllRows() {
		return mRows;
	}

	@Override
	public UserDBRow getUserRow(int zID) {
		for(UserDBRow row : mRows) {
			if(row.getID() == zID) {
				return row;
			}
		}
		return null;
	}

	@Override
	public UserDBRow addUserRow(int zID) {
		UserDBRow row = new JavaUserDBRow(zID);
		mRows.add(row);
		return row;
	}

	@Override
	public void deleteUserRow(int zID) {
		UserDBRow row = getUserRow(zID);
		mRows.remove(row);
	}
	
	@Override
	public ArrayList<Address> getSimpleAddresses() {
		return mAddresses;
	}
	
	@Override
	public Address newSimpleAddress() {
		return newSimpleAddress(GlobalParams.MINIMA_DEFAULT_HASH_STRENGTH);
	}
	
	@Override
	public Address newSimpleAddress(int zBitLength) {
		return newSimpleAddress(new PubPrivKey(zBitLength));
	}
	
	@Override
	public Address newSimpleAddress(PubPrivKey zPubPriv) {
		//Store it..
		mPubPrivKeys.add(zPubPriv);
		
		//A simple script.. 
		String script = "RETURN SIGNEDBY ( "+zPubPriv.getPublicKey()+" )";
		Address addr  = new Address(script, zPubPriv.getBitLength());
		
		//Add to the simple wallet
		mAddresses.add(addr);
		
		//Add to the Total
		mTotalAddresses.add(addr);
		
		return addr;
	}
	

	@Override
	public boolean isSimpleAddress(MiniData zAddress) {
		for(Address addr : mAddresses) {
			if(addr.isEqual(zAddress)) {
				return true;
			}
		}
		
		return false;
	}
	
	@Override
	public PubPrivKey getPubPrivKey(MiniData zPubKey) {
		for(PubPrivKey key : mPubPrivKeys) {
			if(key.getPublicKey().isEqual(zPubKey)) {
				return key;
			}
		}
		
		return null;
	}

	@Override
	public ArrayList<Address> getScriptAddresses() {
		return mScriptAddresses;
	}

	@Override
	public Address newScriptAddress(String zScript) {
		//A simple script.. 
		Address addr = new Address(zScript);
		
		//Do we allready have it ?
		if(isAddressRelevant(addr.getAddressData())) {
			//We have it..
			return addr;
		}
		
		//Add to the simple wallet
		mScriptAddresses.add(addr);
		mTotalAddresses.add(addr);
		
		return addr;
	}
	
	@Override
	public String getScript(MiniData zAddress) {
		//Check the Addresses
		for(Address addr : mTotalAddresses) {
			if(addr.isEqual(zAddress)) {
				return addr.getScript();
			}
		}
		
		return "";
	}

	
	@Override
	public boolean isAddressRelevant(MiniData zAddress) {
		for(Address addr : mTotalAddresses) {
			if(addr.isEqual(zAddress)) {
				return true;
			}
		}
		
		return false;
	}

	@Override
	public boolean isTransactionRelevant(Transaction zTrans) {
		ArrayList<Coin> ins  = zTrans.getAllInputs();
		ArrayList<Coin> outs = zTrans.getAllOutputs();
		
		//Check them - adding the script to outputs we own
		boolean rel = false;
		for(Coin in : ins) {
			if(isAddressRelevant(in.getAddress())) {
				rel = true;
			}
		}
			
		for(Coin out : outs) {
			if(isAddressRelevant(out.getAddress())) {
				rel = true;
			}
		}
		
		return rel;
	}

	@Override
	public MiniData getPublicKey(MiniData zAddress) {
		for(Address addr : mAddresses) {
			if(addr.isEqual(zAddress)) {
				//What is the Public key!
				String script = addr.getScript();
				int index = script.indexOf("0x");
				int end   = script.indexOf(" ", index);
				
				String pubk = script.substring(index, end);
				
				return new MiniData(pubk);
			}
		}
		return null;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int len =0;
		
		//Pub priv keys
		len = mPubPrivKeys.size();
		zOut.writeInt(len);
		for(PubPrivKey key : mPubPrivKeys) {
			key.writeDataStream(zOut);
		}
		
		//Addresses..
		len = mAddresses.size();
		zOut.writeInt(len);
		for(Address addr : mAddresses) {
			addr.writeDataStream(zOut);
		}
		
		//Script Addresses..
		len = mScriptAddresses.size();
		zOut.writeInt(len);
		for(Address addr : mScriptAddresses) {
			addr.writeDataStream(zOut);
		}
		
		//Token Details
		len = mAllTokens.size();
		zOut.writeInt(len);
		for(TokenProof td : mAllTokens) {
			td.writeDataStream(zOut);
		}
		
		//transactions..
		zOut.writeInt(mCounter);
		
		len = mRows.size();
		zOut.writeInt(len);
		for(UserDBRow row : mRows) {
			JavaUserDBRow jrow = (JavaUserDBRow) row;		
			jrow.writeDataStream(zOut);
		}	
		
		//History
		len = mHistory.size();
		zOut.writeInt(len);
		for(reltxpow rtxpow : mHistory) {
			rtxpow.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//reset
		mPubPrivKeys     = new ArrayList<>();
		mAddresses       = new ArrayList<>();
		mScriptAddresses = new ArrayList<>();
		mTotalAddresses  = new ArrayList<>();
		mRows            = new ArrayList<>();	
		mAllTokens		 = new ArrayList<>();
		
		//Pub Priv Keys
		int len = zIn.readInt();
		for(int i=0;i<len;i++) {
			PubPrivKey pp = new PubPrivKey();
			pp.readDataStream(zIn);
			mPubPrivKeys.add(pp);
		}
		
		//Address
		len = zIn.readInt();
		for(int i=0;i<len;i++) {
			Address addr = new Address();
			addr.readDataStream(zIn);
			mAddresses.add(addr);
			mTotalAddresses.add(addr);
		}
		
		//Script Address
		len = zIn.readInt();
		for(int i=0;i<len;i++) {
			Address addr = new Address();
			addr.readDataStream(zIn);
			mScriptAddresses.add(addr);
			mTotalAddresses.add(addr);
		}
		
		//Token Details
		len = zIn.readInt();
		for(int i=0;i<len;i++) {
			mAllTokens.add(TokenProof.ReadFromStream(zIn));
		}
		
		//transaction..
		mCounter = zIn.readInt();
		
		len = zIn.readInt();
		for(int i=0;i<len;i++) {
			JavaUserDBRow row = new JavaUserDBRow();
			row.readDataStream(zIn);
			mRows.add(row);
		}
		
		//History
		mHistory = new ArrayList<reltxpow>();
		len = zIn.readInt();
		for(int i=0;i<len;i++) {
			reltxpow rpow = new reltxpow();
			rpow.readDataStream(zIn);
			mHistory.add(rpow);
		}
	}

	@Override
	public ArrayList<TokenProof> getAllKnownTokens() {
		return mAllTokens;
	}

	@Override
	public TokenProof getTokenDetail(MiniData zTokenID) {
		for(TokenProof td : mAllTokens) {
			if(td.getTokenID().isEqual(zTokenID)) {
				return td;
			}
		}
		
		return null;
	}

	@Override
	public void addTokenDetails(TokenProof zToken) {
		//Check if we have it..
		if(getTokenDetail(zToken.getTokenID()) == null) {
			//We don't have it - add it
			mAllTokens.add(zToken);	
		}
	}

	/**
	 * Transasction History 
	 */
	@Override
	public ArrayList<reltxpow> getHistory() {
		return mHistory;
	}

	@Override
	public void addToHistory(TxPOW zTxPOW, Hashtable<String, MiniNumber> zValues) {
		mHistory.add(new reltxpow( zTxPOW, zValues));
	}

//	@Override
//	public void removeHistory(MiniData zTxPowID) {
//		ArrayList<TxPOW> newhist = new ArrayList<TxPOW>();
//		
//		for(TxPOW txpow : mHistory) {
//			if(!txpow.getTxPowID().isExactlyEqual(zTxPowID)) {
//				newhist.add(txpow);
//			}
//		}
//		
//		mHistory = newhist;
//	}

	@Override
	public void clearHistory() {
		mHistory.clear();
	}

}
