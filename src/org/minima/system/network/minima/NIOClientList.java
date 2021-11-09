package org.minima.system.network.minima;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.base.MiniData;

public class NIOClientList {

	Hashtable<String, NIOClient> mClients;
	
	public NIOClientList() {
		mClients = new Hashtable<>();
	}
	
	public int getNetClientSize() {
		return mClients.size();
	}
	
	public ArrayList<NIOClient> getAllNIOClients(){
		ArrayList<NIOClient> allclients = new ArrayList<>();
		
		Enumeration<NIOClient> clients = mClients.elements();
		while(clients.hasMoreElements()) {
			allclients.add(clients.nextElement());
		}
		
		return allclients;
	}
	
	public void sendMessage(String zUID, MiniData zData) {
		NIOClient client =  mClients.get(zUID);
		if(client != null) {
			client.sendData(zData);
		}
	}
	
	public void sendMessageAll(MiniData zData) {
		Enumeration<NIOClient> clients = mClients.elements();
		while(clients.hasMoreElements()) {
			clients.nextElement().sendData(zData);
		}
	}
	
	public void setWelcome(String zUID, String zWelcome) {
		NIOClient client =  mClients.get(zUID);
		if(client != null) {
			client.setWelcomeMessage(zWelcome);
		}
	}
	
	public void addClient(String zUID, NIOClient zClient) {
		mClients.put(zUID, zClient);
	}
	
	public NIOClient getClient(String zUID) {
		return mClients.get(zUID);
	}
	
	public void removeClient(String zUID) {
		mClients.remove(zUID);
	}
}
