package org.minima.system.commands;

import java.util.ArrayList;
import java.util.Set;
import java.util.StringTokenizer;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.Main;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class CommandRunner {
	
	private CommandRunner() {}
	
	public static CommandRunner getRunner() {
		return new CommandRunner();
	}
	
	public JSONArray runMultiCommand(String zCommand) {
		return runMultiCommand("0x00", zCommand);
	}
	
	public JSONObject runSingleCommand(String zCommand) {
		JSONArray res 		= runMultiCommand(zCommand);
		JSONObject result 	= (JSONObject) res.get(0);
		return result;
	}
	
	public JSONArray runMultiCommand(String zMiniDAPPID, String zCommand) {
		
		JSONArray finalresult = new JSONArray();
		
		//First break it up..
		StringTokenizer strtok = new StringTokenizer(zCommand, ";");
		while(strtok.hasMoreTokens()) {
			String command = strtok.nextToken().trim();
			
			//Get this command..
			Command cmd = Command.getCommand(command);
			
			//Set who called it
			cmd.setMiniDAPPID(zMiniDAPPID);
			
			//The final result
			JSONObject result = null;
			
			//Check the Parameters
			ArrayList<String> validparams 	= cmd.getValidParams();
			
			JSONObject allparams 			=  cmd.getParams();
			Set<String> keys 				= allparams.keySet(); 
			
			boolean validp=true;
			for(String key : keys) {
				if(!validparams.contains(key)) {
					
					//Invalid Param
					result=  new JSONObject();
					result.put("command", command);
					result.put("params", allparams);
					result.put("status", false);
					result.put("pending", false);
					result.put("error", "Invalid parameter : "+key);
					
					//Add to the List..
					finalresult.add(result);
					
					//And that's all folks..
					validp=false;
					break;
				}
			}
			
			//Are we valid..
			if(!validp) {
				break;
			}
			
			//Is this a MiniDAPP..
			if(!zMiniDAPPID.equals("0x00")) {
			
				//What is the command
				String comname = cmd.getName();
				
				//Check this MiniDAPP can make this call..
				boolean allowed = Command.isCommandAllowed(comname);
				
				if(!allowed) {
					
					//Get that MiniDAPP..
					MiniDAPP md = MinimaDB.getDB().getMDSDB().getMiniDAPP(zMiniDAPPID);
					
					//Does it have WRITE permission..
					if(!md.getPermission().equalsIgnoreCase("write")) {
					
						//Add to pending..
						String puid = Main.getInstance().getMDSManager().addPendingCommand(md, command);
						
						//And return..
						result=  new JSONObject();
						result.put("command", command);
						result.put("status", false);
						result.put("pending", true);
						result.put("pendinguid", puid);
						result.put("error", "This command needs to be confirmed and is now pending..");
						
						//Add to the List..
						finalresult.add(result);
						
						//And that's all folks..
						break;
					}
				}
			}
			
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
				result.put("error", exc.toString());
			}
			
			//Add it..
			finalresult.add(result);
			
			//Stop at a false..
			if((boolean)result.get("status") == false) {
				break;
			}
		}
		
		return finalresult;
	}
	
	/*public static Command getCommandOnly(String zCommandName) {
		int commandlen = Command.ALL_COMMANDS.length;
		
		Command comms = null;
		for(int i=0;i<commandlen;i++) {
			if(Command.ALL_COMMANDS[i].getName().equals(zCommandName)) {
				comms = Command.ALL_COMMANDS[i].getFunction();
				break;
			}
		}
	
		return comms;
	}
	
	public static Command getCommand(String zCommand) {
		int commandlen = Command.ALL_COMMANDS.length;
		
		//Get the first word..
		String[] split = splitStringJSON(false,zCommand);
		
		//The first is the command..
		String command = split[0].toLowerCase();
		
		Command comms = null;
		for(int i=0;i<commandlen;i++) {
			if(Command.ALL_COMMANDS[i].getName().equals(command)) {
				comms = Command.ALL_COMMANDS[i].getFunction();
				break;
			}
		}
		
		//If not found return error
		if(comms == null) {
			return new missingcmd(command,"Command not found");
		}
		
		//Set the Complete Command - for reference..
		comms.setCompleteCommand(zCommand);
		
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
				
				//Is this a state variable
				if(command.equals("txnstate")) {

					//Could be a String variable.. add normal String parameter to..
					comms.getParams().put(name, value);

					continue;
				}
				
				//It's a JSONArray..!
				JSONArray json = null;
				try {
					json = (JSONArray) new JSONParser().parse(value);
				} catch (ParseException e) {
					
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
			
			//Check for windows files.. as : screws up
			boolean iswindowsfile = (zInput.indexOf(":\\")!=-1);
			
			//If it's big use the fast one.. but can have issues if : used weirdly.. :( 
			if(!iswindowsfile && zInput.indexOf("{") == -1 && zInput.indexOf("[") == -1) {
				String[] fastres = splitterQuotedPattern(zInput);
				if(fastres != null) {
					return fastres;
				}
			}
		}
		
		//Normal method..
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
	 * Very Fast splitter that doesn't work for JSON inputs.. just normal name value pairs..
	 * 
	 * If something weird happens return null .. and other method used..
	 */
	/*public static String[] splitterQuotedPattern(String zInput) {
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
	    String nvpair 		= new String("");
	    
	    for(String tok : token) {
	    	
	    	if(first){
	    		//First one is just the command
	    		first = false;
	    		finaltokens.add(tok);
	    		
	    	}else {
		    	//Have we found the name yet
		    	if(!namefound) {
		    		
		    		//CHECK ..
		    		if(tok.trim().equals(":")) {
		    			//Hmm. should not HAPPEN!.. use slow method
		    			return null;
		    		}
		    		
		    		namefound 	 = true;
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
	
	/**
	 * Which Commands are WRITE commands..
	 */
	/*private static final String[] ALL_WRITE_COMMANDS = 
		{"send","sendpoll","sendsign","multisig","tokencreate","consolidate",
		 "cointrack","sign","txnsign","mds","backup","removescript",
		 "restore","restoresync","vault","archive","mysql","mysqlcoins",
		 "rpc","magic","quit","seedrandom","megammrsync"};
	
	private static final ArrayList<String> ALL_WRITE_COMMANDS_ARRAY = new ArrayList<String>(Arrays.asList(ALL_WRITE_COMMANDS));
	
	public static boolean isCommandAllowed(String zCommand) {
		
		//Is it a simple READ command
		if(ALL_WRITE_COMMANDS_ARRAY.contains(zCommand.trim())) {
			return false;
		}
		
		return true;
	}*/
	
}