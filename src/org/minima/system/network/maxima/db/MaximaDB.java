package org.minima.system.network.maxima.db;

import java.util.ArrayList;

public class MaximaDB {

	ArrayList<MaximaUser> mUsers;
	
	public MaximaDB() {
		mUsers = new ArrayList<>();
	}
	
	public MaximaUser getUser(String zPublicKey) {
		for(MaximaUser user : mUsers) {
			if(user.getPublicKey().equalsIgnoreCase(zPublicKey)) {
				return user;
			}
		}
		
		return null;
	}
	
	public void addUser(MaximaUser zUser) {
		mUsers.add(zUser);
	}
	
	
}
