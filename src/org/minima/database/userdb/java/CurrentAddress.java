package org.minima.database.userdb.java;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Random;

import org.minima.GlobalParams;
import org.minima.database.userdb.UserDB;
import org.minima.objects.Address;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.MultiKey;
import org.minima.system.brains.ConsensusBackup;
import org.minima.system.brains.ConsensusHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class CurrentAddress implements Streamable {

	public static int CURRENT_ADDRESS_NUM = 32;
	
	public static MiniNumber MAX_REQUEST = new MiniNumber(4000);
	
	public class CAddressDetails implements Streamable {
		Address 	 mSimpleAddress;
		MiniNumber 	 mRequests;
		
		public CAddressDetails() {
			mRequests = MiniNumber.ZERO;
		}

		public Address getAddress() {
			return mSimpleAddress;
		}
		
		public void setAddress(Address zAddr) {
			mSimpleAddress = zAddr;
		}
		
		public MiniNumber getRequests() {
			return mRequests;
		}
		
		public void incrementRequests() {
			mRequests = mRequests.increment();
		}
		
		@Override
		public void writeDataStream(DataOutputStream zOut) throws IOException {
			mSimpleAddress.writeDataStream(zOut);
			mRequests.writeDataStream(zOut);
		}

		@Override
		public void readDataStream(DataInputStream zIn) throws IOException {
			mSimpleAddress 	= Address.ReadFromStream(zIn);
			mRequests 		= MiniNumber.ReadFromStream(zIn);
		}
	}
	
	MiniNumber mTotalUsed;
	CAddressDetails[] mAddresses; 
	
	public CurrentAddress() {
		mAddresses = new CAddressDetails[CURRENT_ADDRESS_NUM];
		mTotalUsed = MiniNumber.ZERO;
	}
	
	public Address getCurrentAddress(UserDB zUserDB, ConsensusHandler zBackup) {
		//Choose a random key..
		Random rr = new Random();
		int index = rr.nextInt(CURRENT_ADDRESS_NUM);
		
		//Check it's valid..
		boolean createnew = false;
		if(mAddresses[index] == null) {
			createnew = true;
		}else if(mAddresses[index].getRequests().isMoreEqual(MAX_REQUEST)) {
			createnew = true;
		}
			
		//Need a new Key.. ?
		if(createnew) {
			mAddresses[index] = new CAddressDetails();
			
			//Create a new KEY - give 16*16*16 signatures = 4096
			MultiKey key = new MultiKey(GlobalParams.MINIMA_DEFAULT_HASH_STRENGTH, 
					new MiniNumber(16), new MiniNumber(3));
			
			//Create a new address.. with a few thousand uses..
			mAddresses[index].setAddress(zUserDB.newSimpleAddress(key));
			
			//Total addreses used
			mTotalUsed = mTotalUsed.increment();
			
			//Log it..
			MinimaLogger.log("NEW base address created ["+index+" / "+mTotalUsed+"] : "+mAddresses[index].getAddress().getMinimaAddress());
			
			//Backup
			zBackup.PostMessage(ConsensusBackup.CONSENSUSBACKUP_BACKUPUSER);
		}
		
		//Increment uses
		mAddresses[index].incrementRequests();
		
		return mAddresses[index].getAddress();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//How many addresses used
		mTotalUsed.writeDataStream(zOut);
		
		//Now output the current addreses
		for(int i=0;i<CURRENT_ADDRESS_NUM;i++) {
			if(mAddresses[i] == null) {
				MiniByte.FALSE.writeDataStream(zOut);
			}else {
				MiniByte.TRUE.writeDataStream(zOut);
				mAddresses[i].writeDataStream(zOut);
			}
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTotalUsed = MiniNumber.ReadFromStream(zIn);
		
		mAddresses = new CAddressDetails[CURRENT_ADDRESS_NUM];
		for(int i=0;i<CURRENT_ADDRESS_NUM;i++) {
			boolean valid = MiniByte.ReadFromStream(zIn).isTrue();
			if(valid) {
				mAddresses[i] = new CAddressDetails();
				mAddresses[i].readDataStream(zIn);
			}
		}
	}
}
