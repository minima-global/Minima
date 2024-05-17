package org.minima.system.commands;

import java.util.ArrayList;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.backup.archive;
import org.minima.system.commands.backup.backup;
import org.minima.system.commands.backup.decryptbackup;
import org.minima.system.commands.backup.mysql;
import org.minima.system.commands.backup.mysqlcoins;
import org.minima.system.commands.backup.reset;
import org.minima.system.commands.backup.restore;
import org.minima.system.commands.backup.restoresync;
import org.minima.system.commands.backup.vault;
import org.minima.system.commands.backup.mmrsync.megammr;
import org.minima.system.commands.backup.mmrsync.megammrsync;
import org.minima.system.commands.base.automine;
import org.minima.system.commands.base.balance;
import org.minima.system.commands.base.block;
import org.minima.system.commands.base.burn;
import org.minima.system.commands.base.checkaddress;
import org.minima.system.commands.base.coincheck;
import org.minima.system.commands.base.coinexport;
import org.minima.system.commands.base.coinimport;
import org.minima.system.commands.base.coinnotify;
import org.minima.system.commands.base.cointrack;
import org.minima.system.commands.base.consolidate;
import org.minima.system.commands.base.convert;
import org.minima.system.commands.base.debugflag;
import org.minima.system.commands.base.getaddress;
import org.minima.system.commands.base.hash;
import org.minima.system.commands.base.hashtest;
import org.minima.system.commands.base.healthcheck;
import org.minima.system.commands.base.incentivecash;
import org.minima.system.commands.base.logs;
import org.minima.system.commands.base.maths;
import org.minima.system.commands.base.mempool;
import org.minima.system.commands.base.mmrcreate;
import org.minima.system.commands.base.mmrproof;
import org.minima.system.commands.base.newaddress;
import org.minima.system.commands.base.printmmr;
import org.minima.system.commands.base.printtree;
import org.minima.system.commands.base.quit;
import org.minima.system.commands.base.random;
import org.minima.system.commands.base.seedrandom;
import org.minima.system.commands.base.slavenode;
import org.minima.system.commands.base.status;
import org.minima.system.commands.base.test;
import org.minima.system.commands.base.tokencreate;
import org.minima.system.commands.base.tokenvalidate;
import org.minima.system.commands.base.trace;
import org.minima.system.commands.maxima.maxcontacts;
import org.minima.system.commands.maxima.maxcreate;
import org.minima.system.commands.maxima.maxextra;
import org.minima.system.commands.maxima.maxima;
import org.minima.system.commands.maxima.maxsign;
import org.minima.system.commands.maxima.maxverify;
import org.minima.system.commands.mds.checkmode;
import org.minima.system.commands.mds.checkpending;
import org.minima.system.commands.mds.checkrestore;
import org.minima.system.commands.mds.mds;
import org.minima.system.commands.network.connect;
import org.minima.system.commands.network.disconnect;
import org.minima.system.commands.network.message;
import org.minima.system.commands.network.network;
import org.minima.system.commands.network.p2pstate;
import org.minima.system.commands.network.peers;
import org.minima.system.commands.network.ping;
import org.minima.system.commands.network.rpc;
import org.minima.system.commands.network.webhooks;
import org.minima.system.commands.scripts.newscript;
import org.minima.system.commands.scripts.removescript;
import org.minima.system.commands.scripts.runscript;
import org.minima.system.commands.scripts.scripts;
import org.minima.system.commands.search.coins;
import org.minima.system.commands.search.history;
import org.minima.system.commands.search.keys;
import org.minima.system.commands.search.tokens;
import org.minima.system.commands.search.txpow;
import org.minima.system.commands.send.multisig;
import org.minima.system.commands.send.send;
import org.minima.system.commands.send.sendnosign;
import org.minima.system.commands.send.sendpoll;
import org.minima.system.commands.send.sendpost;
import org.minima.system.commands.send.sendsign;
import org.minima.system.commands.send.sendview;
import org.minima.system.commands.send.wallet.createfrom;
import org.minima.system.commands.send.wallet.postfrom;
import org.minima.system.commands.send.wallet.sendfrom;
import org.minima.system.commands.send.wallet.signfrom;
import org.minima.system.commands.signatures.sign;
import org.minima.system.commands.signatures.verify;
import org.minima.system.commands.txn.txnaddamount;
import org.minima.system.commands.txn.txnauto;
import org.minima.system.commands.txn.txnbasics;
import org.minima.system.commands.txn.txncheck;
import org.minima.system.commands.txn.txnclear;
import org.minima.system.commands.txn.txncreate;
import org.minima.system.commands.txn.txndelete;
import org.minima.system.commands.txn.txnexport;
import org.minima.system.commands.txn.txnimport;
import org.minima.system.commands.txn.txninput;
import org.minima.system.commands.txn.txnlist;
import org.minima.system.commands.txn.txnlock;
import org.minima.system.commands.txn.txnmmr;
import org.minima.system.commands.txn.txnoutput;
import org.minima.system.commands.txn.txnpost;
import org.minima.system.commands.txn.txnscript;
import org.minima.system.commands.txn.txnsign;
import org.minima.system.commands.txn.txnstate;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public abstract class Command {
	
	String mName;
	
	String mHelp;
	
	JSONObject mParams = new JSONObject();
	
	String mCompleteCommand = new String("");
	
	String mMiniDAPPID = "";
	
	public Command(String zName, String zHelp) {
		mName = zName;
		mHelp = zHelp;
	}
	
	public void setMiniDAPPID(String zMiniDAPPID) {
		mMiniDAPPID = zMiniDAPPID;
	}
	
	public String getMiniDAPPID() {
		return mMiniDAPPID;
	}
	
	public void setCompleteCommand(String zCommand) {
		mCompleteCommand = zCommand;
	}
	
	public ArrayList<String> getValidParams(){
		return new ArrayList<>();
	}
	
	public String getCompleteCommand() {
		return mCompleteCommand;
	}
	
	public String getHelp() {
		return mHelp;
	}
	
	public String getFullHelp() {
		return mHelp;
	}
	
	public JSONObject getJSONReply() {
		JSONObject json = new JSONObject();
		json.put("command", getName());
		
		//Are they empty..
		if(!getParams().isEmpty()) {
			json.put("params", getParams());
		}
		
		json.put("status", true);
		json.put("pending", false);
		
		return json;
	}
	
	public String getName() {
		return mName;
	}
	
	public JSONObject getParams() {
		return mParams;
	}
	
	public boolean existsParam(String zParamName) {
		return mParams.containsKey(zParamName);
	}
	
	public String getParam(String zParamName) throws CommandException {
		if(!existsParam(zParamName)) {
			throw new CommandException("param not specified : "+zParamName);
		}
		
		//Check for blank
		String pp = (String)mParams.get(zParamName);
		pp = pp.trim();
		if(pp.equals("")) {
			throw new CommandException("BLANK param not allowed : "+zParamName);
		}
		
		return pp;
	}
	
	public String getParam(String zParamName, String zDefault) throws CommandException {
		if(existsParam(zParamName)) {
			//Check for blank
			String pp = (String)mParams.get(zParamName);
			pp = pp.trim();
			if(pp.equals("")) {
				throw new CommandException("BLANK param not allowed : "+zParamName);
			}
			
			return pp;
		}
		
		return zDefault;
	}
	
	public boolean getBooleanParam(String zParamName) throws CommandException {
		String bool = getParam(zParamName);
		if(bool.equals("true")){
			return  true;
		}
		return false;
	}
	
	public boolean getBooleanParam(String zParamName, boolean zDefault) throws CommandException {
		if(existsParam(zParamName)) {
			if(getParam(zParamName).equals("true")){
				return  true;
			}else {
				return false;
			}
		}
		
		return zDefault;
	}
	
	public MiniNumber getNumberParam(String zParamName) throws CommandException {
		String num = getParam(zParamName);
		return new MiniNumber(num);
	}
	
	public MiniNumber getNumberParam(String zParamName, MiniNumber zDefault) throws CommandException {
		if(existsParam(zParamName)) {
			return getNumberParam(zParamName);
		}
		return zDefault;
	}
	
	public MiniData getDataParam(String zParamName) throws CommandException {
		String hex = getParam(zParamName);
		return new MiniData(hex);
	}
	
	public JSONObject getJSONObjectParam(String zParamName) throws CommandException{
		if(!existsParam(zParamName)) {
			throw new CommandException("param not specified : "+zParamName);
		}
		
		return (JSONObject) mParams.get(zParamName);
	}
	
	public JSONArray getJSONArrayParam(String zParamName) throws CommandException {
		if(!existsParam(zParamName)) {
			throw new CommandException("param not specified : "+zParamName);
		}
		
		return (JSONArray) mParams.get(zParamName);
	}
	
	public String getAddressParam(String zParamName, String zDefault) throws CommandException {
		if(existsParam(zParamName)) {
			return getAddressParam(zParamName);
		}
		
		return zDefault;
	}
	
	public String getAddressParam(String zParamName) throws CommandException {
		if(!existsParam(zParamName)) {
			throw new CommandException("param not specified : "+zParamName);
		}

		String address = getParam(zParamName);
		if(address.toLowerCase().startsWith("mx")) {
			//Convert back to normal hex..
			try {
				address = Address.convertMinimaAddress(address).to0xString();
			}catch(IllegalArgumentException exc) {
				throw new CommandException(exc.toString());
			}
		}
		
		//If it's an 0x address check converts to MiniData correctly
		if(address.startsWith("0x")) {
			try {
				MiniData data 	= new MiniData(address);
				address 		= data.to0xString();
			}catch(Exception exc) {
				throw new CommandException(exc.toString());
			}
		}
		
		return address;
	}
	
	public boolean isParamJSONObject(String zParamName) {
		if(existsParam(zParamName)) {
			Object obj = mParams.get(zParamName);
			if(obj instanceof JSONObject) {
				return true;
			}
		}
		
		return false;
	}
	
	public boolean isParamJSONArray(String zParamName) {
		if(existsParam(zParamName)) {
			Object obj = mParams.get(zParamName);
			if(obj instanceof JSONArray) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * These function need to be implemented..
	 */
	
	public abstract JSONObject runCommand() throws Exception;
	
	public abstract Command getFunction();	
}
