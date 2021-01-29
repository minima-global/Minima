package org.minima.system.network.maxima.db;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.MultiKey;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MaximaDB implements Streamable {

	MultiKey mAccount;
	
	ArrayList<MaximaUser> mUsers;
	
	public MaximaDB() {
		mUsers = new ArrayList<>();
	}
	
	public void newAccount() {
		mAccount = new MultiKey(160, new MiniNumber(32), new MiniNumber(3));
	}
	
	public MultiKey getAccount() {
		return mAccount;
	}
	
	public MaximaUser getUser(String zPublicKey) {
		for(MaximaUser user : mUsers) {
			if(user.getPublicKey().equalsIgnoreCase(zPublicKey)) {
				return user;
			}
		}
		
		return null;
	}
	
	public MaximaUser findUser(String zPublicKey) {
		for(MaximaUser mx : mUsers) {
			if(mx.getPublicKey().equalsIgnoreCase(zPublicKey)) {
				return mx;
			}
		}
		
		return null;
	}
	
	public void addUser(MaximaUser zUser) {
		mUsers.add(zUser);
	}
	
	public ArrayList<MaximaUser> getAllUsers(){
		return mUsers;
	}
	
	public void saveDB(File zFile) {
		//Save the MaximaDB
		try {
			FileOutputStream fos = new FileOutputStream(zFile);
			DataOutputStream dos = new DataOutputStream(fos);
			
			writeDataStream(dos);
			dos.flush();
			
			dos.close();
			fos.close();
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
	}
	
	public void loadDB(File zFile) {
		//Get the MaximaDB
		try {
			FileInputStream fis = new FileInputStream(zFile);
			DataInputStream dis = new DataInputStream(fis);
			
			readDataStream(dis);
			
			dis.close();
			fis.close();
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
	}


	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Write out the Pub Key
		mAccount.writeDataStream(zOut);
		
		//Write out the users
		MiniNumber len = new MiniNumber(mUsers.size());
		len.writeDataStream(zOut);
		for(MaximaUser user : mUsers) {
			user.writeDataStream(zOut);
		}
	}


	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//Read in the key
		mAccount = new MultiKey();
		mAccount.readDataStream(zIn);
		
		//Read in the User DB
		mUsers = new ArrayList();
		MiniNumber minlen = MiniNumber.ReadFromStream(zIn);
		int len = minlen.getAsInt();
		for(int i=0;i<len;i++) {
			MaximaUser user = new MaximaUser();
			user.readDataStream(zIn);
			mUsers.add(user);
		}
	}
}
