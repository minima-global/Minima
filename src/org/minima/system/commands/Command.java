package org.minima.system.commands;

import java.util.ArrayList;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
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
