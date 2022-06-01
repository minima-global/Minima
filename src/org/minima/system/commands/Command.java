package org.minima.system.commands;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.base.*;
import org.minima.system.commands.maxima.maxcontacts;
import org.minima.system.commands.maxima.maxima;
import org.minima.system.commands.network.connect;
import org.minima.system.commands.network.disconnect;
import org.minima.system.commands.network.message;
import org.minima.system.commands.network.network;
import org.minima.system.commands.network.ping;
import org.minima.system.commands.network.rpc;
import org.minima.system.commands.network.webhooks;
import org.minima.system.commands.persistent.file;
import org.minima.system.commands.persistent.sql;
import org.minima.system.commands.scripts.newscript;
import org.minima.system.commands.scripts.runscript;
import org.minima.system.commands.scripts.scripts;
import org.minima.system.commands.search.coins;
import org.minima.system.commands.search.keys;
import org.minima.system.commands.search.tokens;
import org.minima.system.commands.search.txpow;
import org.minima.system.commands.signatures.sign;
import org.minima.system.commands.signatures.verify;
import org.minima.system.commands.txn.txnbasics;
import org.minima.system.commands.txn.txncheck;
import org.minima.system.commands.txn.txnclear;
import org.minima.system.commands.txn.txncreate;
import org.minima.system.commands.txn.txndelete;
import org.minima.system.commands.txn.txnexport;
import org.minima.system.commands.txn.txnimport;
import org.minima.system.commands.txn.txninput;
import org.minima.system.commands.txn.txnlist;
import org.minima.system.commands.txn.txnoutput;
import org.minima.system.commands.txn.txnpost;
import org.minima.system.commands.txn.txnscript;
import org.minima.system.commands.txn.txnsign;
import org.minima.system.commands.txn.txnstate;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public abstract class Command {

	public static final Command[] ALL_COMMANDS = 
		{   new quit(), new status(), new coins(), new txpow(), new connect(), new disconnect(), new network(),
			new message(), new trace(), new help(), new printtree(), new automine(), new printmmr(), new rpc(),
			new send(), new balance(), new tokencreate(), new tokenvalidate(), new tokens(),new getaddress(), new newaddress(), new debugflag(),
			new incentivecash(), new webhooks(), new peers(), new p2pstate(),

			//Removed code..
//			new sshtunnel(), 
			
			new ping(),
			new sql(),new file(),
			new vault(), new consolidate(),
			new backup(), new restore(), new test(), 
			new runscript(), new tutorial(),new keys(),
			new scripts(), new newscript(),
			new burn(),
			
			new txnbasics(),new txncreate(), new txninput(),new txnlist(), new txnclear(),
			new txnoutput(),new txnstate(),new txnsign(),new txnpost(),new txndelete(),
			new txnexport(),new txnimport(),new txncheck(), new txnscript(),
			
			new coinimport(), new coinexport(),new cointrack(), new coincheck(),
			
			new hash(), new hashtest(), new sign(), new verify(),
			
			new maxima(), new maxcontacts(),
			
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
	
	public String getFullHelp() {
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
	
	public String getParam(String zParamName) throws CommandException {
		if(!existsParam(zParamName)) {
			throw new CommandException("param not specified : "+zParamName);
		}
		
		return (String) mParams.get(zParamName);
	}
	
	public String getParam(String zParamName, String zDefault) {
		if(existsParam(zParamName)) {
			return (String) mParams.get(zParamName);
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
			
			}catch(CommandException cexc) {
				result = cmd.getJSONReply();
				result.put("status", false);
				result.put("error", cexc.getMessage());
				
			}catch(Exception exc) {
				//Print the full error
				MinimaLogger.log(exc);
				
				result = cmd.getJSONReply();
				result.put("status", false);
				result.put("error", exc.getMessage());
			}
			
			//Add it..
			res.add(result);
			
			//Stop at a false..
			if((boolean)result.get("status") == false) {
				break;
			}
		}
		
		return res;
	}
	
	public static Command getCommand(String zCommand) {
		int commandlen = ALL_COMMANDS.length;
		
		//Get the first word..
		String[] split = splitStringJSON(false,zCommand);
		
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
					
					//Is this a state variable
					if(command.equals("txnstate")) {
						
						//Could be a String variable.. add normal String parameter to..
						comms.getParams().put(name, value);
						
						continue;
					}
					
					//Otherwise is just a broken JSONArray
					return new missingcmd(command,"Invalid JSON parameter for "+command+" @ "+token+" "+e.toString());
				}
				
				//Store this parameter..
				comms.getParams().put(name, json);
				
			}else {
				
				//Should not have any "..
				if(value.startsWith("\"")) {
					value = value.substring(1);
				}
				
				if(value.endsWith("\"")) {
					value = value.substring(0,value.length()-1);
				}
				
				//Add normal String parameter to..
				comms.getParams().put(name, value);
			}
		}
		
		return comms;
	}
	
	private static String[] splitStringJSON(boolean zForceNormal, String zInput) {
		//Are there any JSON in this.. if not use super fast method..
		if(!zForceNormal) {
			if(zInput.indexOf("{") == -1 && zInput.indexOf("[") == -1) {
				return splitterQuotedPattern(zInput);
			}
		}
		
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
	
	/**
	 * Very Fast splitter that doesn;t work for JSON inputs.. just normal name value pairs..
	 */
	public static String[] splitterQuotedPattern(String zInput) {
		ArrayList<String> token = new ArrayList<>();
		
		//Split the : name value pairs aswell..
		String ss = zInput.trim().replaceAll(":", " : ");
		
		//Search for quoted strings
		String regex 	= "\"([^\"]*)\"|(\\S+)";
		Matcher m 		= Pattern.compile(regex).matcher(ss);
	    while (m.find()) {
	        if (m.group(1) != null) {
	        	token.add(m.group(1));
	        } else {
	        	token.add(m.group(2));
	        }
	    }
		
	    //Now cycle and add up the name:value pairs
	    ArrayList<String> finaltokens = new ArrayList<>();
	    boolean first 		= true; 
	    boolean namefound	= false;
	    String nvpair = "";
	    for(String tok : token) {
	    	
	    	//First one is just the command
	    	if(first){
	    		first = false;
	    		finaltokens.add(tok);
	    	}else {
		    	//Have we found the name yet
		    	if(!namefound) {
		    		namefound 	= true;
		    		nvpair 		= new String(tok);
		    	}else {
		    		if(tok.trim().equals(":")) {
		    			nvpair+=":";
		    		}else {
		    			namefound 	 = false;
		    			nvpair		+=tok;
		    			
		    			//Add to the final list
		    			finaltokens.add(nvpair);
		    		}
		    	}
	    	}
	    }
	    
		return finaltokens.toArray(new String[0]);
	}
	
//	public static String[] splitterQuotedPattern(String zInput) {
//		ArrayList<String> token = new ArrayList<>();
//		String ss = zInput.trim();
//		
//		String regex = "\"([^\"]*)\"|(\\S+)";
//		
//		Matcher m = Pattern.compile(regex).matcher(ss);
//	    while (m.find()) {
//	        if (m.group(1) != null) {
//	        	token.add(m.group(1));
//	        } else {
//	        	token.add(m.group(2));
//	        }
//	    }
//	   
//		return token.toArray(new String[0]);
//	}
	
	public static void main(String[] zArgs) {
		
//		String tester2 = "runscript script:\"RETURN TRUE\" state:{\"0\":\"123\"}";
		
		String tester2 = "runscript script:\"RETURN TRUE\" data:0x00 application:\"max solo yolo\"";
		//String tester2 = "maxima action:setname name:\"Spartacus Rex\"";

		System.out.println("\nOLD WAY\n");
		String [] split = splitStringJSON(true, tester2);
		for(int i=0;i<split.length;i++) {
			System.out.println(i+") "+split[i]);
		}
		
		System.out.println("\nNEW WAY\n");
		
		split = splitterQuotedPattern(tester2);
		for(int i=0;i<split.length;i++) {
			System.out.println(i+") "+split[i]);
		}
	}
}
