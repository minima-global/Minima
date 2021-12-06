package org.minima.system.commands;

import java.util.ArrayList;
import java.util.StringTokenizer;

import org.minima.system.commands.all.automine;
import org.minima.system.commands.all.backup;
import org.minima.system.commands.all.balance;
import org.minima.system.commands.all.coins;
import org.minima.system.commands.all.connect;
import org.minima.system.commands.all.debugflag;
import org.minima.system.commands.all.disconnect;
import org.minima.system.commands.all.getaddress;
import org.minima.system.commands.all.hashtest;
import org.minima.system.commands.all.help;
import org.minima.system.commands.all.incentivecash;
import org.minima.system.commands.all.message;
import org.minima.system.commands.all.missingcmd;
import org.minima.system.commands.all.mmrcreate;
import org.minima.system.commands.all.mmrproof;
import org.minima.system.commands.all.network;
import org.minima.system.commands.all.newaddress;
import org.minima.system.commands.all.printmmr;
import org.minima.system.commands.all.printtree;
import org.minima.system.commands.all.quit;
import org.minima.system.commands.all.restore;
import org.minima.system.commands.all.rpc;
import org.minima.system.commands.all.runscript;
import org.minima.system.commands.all.send;
import org.minima.system.commands.all.sshtunnel;
import org.minima.system.commands.all.status;
import org.minima.system.commands.all.test;
import org.minima.system.commands.all.tokencreate;
import org.minima.system.commands.all.tokens;
import org.minima.system.commands.all.trace;
import org.minima.system.commands.all.tutorial;
import org.minima.system.commands.all.txpow;
import org.minima.system.commands.all.webhooks;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public abstract class Command {

	public static final Command[] ALL_COMMANDS = 
		{   new quit(), new status(), new coins(), new txpow(), new connect(), new disconnect(), new network(),
			new message(), new trace(), new help(), new printtree(), new automine(), new printmmr(), new rpc(),
			new send(), new balance(), new tokencreate(), new tokens(),new getaddress(), new newaddress(), new debugflag(),
			new incentivecash(), new sshtunnel(), new webhooks(),
			new backup(), new restore(), new test(), new hashtest(),
			new runscript(), new tutorial(), 
			new mmrcreate(), new mmrproof()};
	
	String mName;
	String mHelp;
	
	JSONObject mParams = new JSONObject();
	
	public Command(String zName, String zHelp) {
		mName = zName;
		mHelp = zHelp;
	}
	
	public String getHelp() {
		return mHelp;
	}
	
	public JSONObject getJSONReply() {
		JSONObject json = new JSONObject();
		json.put("command", getname());
		
		//Are they empty..
		if(!getParams().isEmpty()) {
			json.put("params", getParams());
		}
		
		json.put("status", true);
		return json;
	}
	
	public String getname() {
		return mName;
	}
	
	public JSONObject getParams() {
		return mParams;
	}
	
	public boolean existsParam(String zParamName) {
		return mParams.containsKey(zParamName);
	}
	
	public String getParam(String zParamName) {
		return getParam(zParamName, "");
	}
	
	public String getParam(String zParamName, String zDefault) {
		if(existsParam(zParamName)) {
			return (String) mParams.get(zParamName);
		}
		
		return zDefault;
	}
	
	public JSONObject getJSONObjectParam(String zParamName) {
		return (JSONObject) mParams.get(zParamName);
	}
	
	public JSONArray getJSONArrayParam(String zParamName) {
		return (JSONArray) mParams.get(zParamName);
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
	
	
	public abstract JSONObject runCommand() throws Exception;
	
	public abstract Command getFunction();
	
	/**
	 * Run a with possible multiple functions
	 * 
	 * @param zCommand
	 */
	public static JSONArray runMultiCommand(String zCommand) {
		JSONArray res = new JSONArray();
		
		//First break it up..
		StringTokenizer strtok = new StringTokenizer(zCommand, ";");
		while(strtok.hasMoreTokens()) {
			String command = strtok.nextToken().trim();
			
			//Run this command..
			Command cmd = Command.getCommand(command);
			
			//Run it..
			JSONObject result = null;
			try {
				result = cmd.runCommand();
			}catch(Exception exc) {
				MinimaLogger.log(exc);
				
				result = cmd.getJSONReply();
				result.put("status", false);
				result.put("error", exc.getMessage());
			}
			
			//Add it..
			res.add(result);
		}
		
		return res;
	}
	
	public static Command getCommand(String zCommand) {
		int commandlen = ALL_COMMANDS.length;
		
		//Get the first word..
//		String[] split = splitString(zCommand);
		String[] split = splitStringJSON(zCommand);
		
		//The first is the command..
		String command = split[0];
		
		Command comms = null;
		for(int i=0;i<commandlen;i++) {
			if(ALL_COMMANDS[i].getname().equals(command)) {
				comms = ALL_COMMANDS[i].getFunction();
				break;
			}
		}
		
		//If not found return error
		if(comms == null) {
			return new missingcmd(command,"Command not found");
		}
		
		//get the parameters if any
		int len = split.length;
		for(int i=1;i<len;i++) {
			String token = split[i];
			
			//Find the :
			int index 	 = token.indexOf(":");
			if(index == -1) {
				return new missingcmd(command,"Invalid parameters for "+command+" @ "+token);
			}
			
			
			String name  = token.substring(0, index).trim();
			String value = token.substring(index+1).trim();
			
			//Is the value a JSON..or JSONArray..
			if(value.startsWith("{") && value.endsWith("}")) {
				
				//It's a JSON..!
				JSONObject json = null;
				try {
					json = (JSONObject) new JSONParser().parse(value);
				} catch (ParseException e) {
					return new missingcmd(command,"Invalid JSON parameter for "+command+" @ "+token+" "+e.toString());
				}
				
				//Store this parameter..
				comms.getParams().put(name, json);
			
			}else if(value.startsWith("[") && value.endsWith("]")) {
				
				//It's a JSONArray..!
				JSONArray json = null;
				try {
					json = (JSONArray) new JSONParser().parse(value);
				} catch (ParseException e) {
					return new missingcmd(command,"Invalid JSON parameter for "+command+" @ "+token+" "+e.toString());
				}
				
				//Store this parameter..
				comms.getParams().put(name, json);
				
			}else {
				
				//Add normal String parameter to..
				comms.getParams().put(name, value);
				
			}
			
			
		}
		
		return comms;
	}

	/**
	 * Split the input string keeping quoted sections as single units
	 * 
	 * @param zString
	 * @return
	 */
	private static String[] splitString(String zInput) {
		ArrayList<String> token = new ArrayList<>();
		String ss = zInput.trim();
		
		//Cycle through looking for spaces or quotes..
		String current = new String();
		boolean quoted = false;
		int len = ss.length();
		for(int i=0;i<len;i++) {
			char cc = ss.charAt(i);
			
			if(cc == ' ') {
				//End of the line..
				if(!quoted) {
					//Add current
					if(!current.equals("")) {
						token.add(current.trim());
					}
						
					//New Current
					current = new String();
				}else {
					current += cc;
				}
			}else if(cc == '\"') {
				//it's a quote!
				if(quoted) {
					//It's finished..
					quoted=false;
				}else {
					quoted=true;
				}
			}else {
				current += cc;
			}
		}
		
		//Add the last bit..
		if(!current.equals("")) {
			token.add(current.trim());
		}
		
		return token.toArray(new String[0]);
	}
	
	private static String[] splitStringJSON(String zInput) {
		ArrayList<String> token = new ArrayList<>();
		String ss = zInput.trim();
		
		//Cycle through looking for spaces or quotes..
		String current = new String();
		
		int jsoned 		= 0;
		boolean quoted 	= false;
		
		int len = ss.length();
		for(int i=0;i<len;i++) {
			char cc = ss.charAt(i);
			
			if(cc == ' ') {
				//End of the line..
				if(!quoted && jsoned==0) {
					
					//Add current
					if(!current.equals("")) {
						token.add(current.trim());
					}
						
					//New Current
					current = new String();
					
				}else {
					current += cc;
				}
			
			}else if(cc == '{') {
				jsoned++;
				current += cc;
				
			}else if(cc == '}') {
				jsoned--;
				current += cc;
			
			}else if(cc == '[') {
				jsoned++;
				current += cc;
				
			}else if(cc == ']') {
				jsoned--;
				current += cc;
			
			
			}else if(cc == '\"') {
				if(jsoned>0) {
					
					//It's in a JSON.. so keep it..
					current += cc;
					
				}else {
					//it's a quote!
					if(quoted) {
						//It's finished..
						quoted=false;
					}else {
						quoted=true;
					}
				}
				
			}else {
				current += cc;
			}
		}
		
		//Add the last bit..
		if(!current.equals("")) {
			token.add(current.trim());
		}
		
		return token.toArray(new String[0]);
	}
}
